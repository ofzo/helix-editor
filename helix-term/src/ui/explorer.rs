use super::Prompt;
use crate::{
    compositor::{Component, Context, EventResult},
    ctrl, key, shift, ui,
    ui::PromptEvent,
};
use anyhow::{bail, ensure, Result};
use helix_core::Position;
use helix_view::{
    editor::{Action, ExplorerPosition},
    graphics::{CursorKind, Rect},
    info::Info,
    input::{Event, KeyCode, KeyEvent, KeyModifiers},
    theme::Style,
    Editor,
};
use helix_vcs::FileChangeStatus;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::mpsc::Receiver;

use std::borrow::Cow;
use tree::{TreeOp, TreeView, TreeViewItem};
use tui::{
    buffer::Buffer as Surface,
    widgets::{Block, Borders, Widget},
};

mod tree;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
enum FileType {
    File,
    Folder,
    Root,
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct FileInfo {
    file_type: FileType,
    path: PathBuf,
    is_ignored: bool,  // 标记文件是否被 gitignore
    is_symlink: bool,
}

impl FileInfo {
    fn root(path: PathBuf) -> Self {
        Self {
            file_type: FileType::Root,
            path,
            is_ignored: false,
            is_symlink: false,
        }
    }

    fn get_text(&self) -> Cow<'static, str> {
        let text = match self.file_type {
            // FileType::Root => self.path.display().to_string(),
            FileType::Root | FileType::File | FileType::Folder => self
                .path
                .file_name()
                .map_or("/".into(), |p| p.to_string_lossy().into_owned()),
        };

        #[cfg(test)]
        let text = text.replace(std::path::MAIN_SEPARATOR, "/");

        text.into()
    }
}

impl PartialOrd for FileInfo {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FileInfo {
    fn cmp(&self, other: &Self) -> Ordering {
        use FileType::*;
        match (self.file_type, other.file_type) {
            (Root, _) => return Ordering::Less,
            (_, Root) => return Ordering::Greater,
            _ => {}
        };

        if let (Some(p1), Some(p2)) = (self.path.parent(), other.path.parent()) {
            if p1 == p2 {
                match (self.file_type, other.file_type) {
                    (Folder, File) => return Ordering::Less,
                    (File, Folder) => return Ordering::Greater,
                    _ => {}
                };
            }
        }
        self.path.cmp(&other.path)
    }
}

impl TreeViewItem for FileInfo {
    type Params = State;

    fn get_children(&self, state: &State) -> Result<Vec<Self>> {
        match self.file_type {
            FileType::Root | FileType::Folder => {}
            _ => return Ok(vec![]),
        };

        // Collect entries, filtering .git
        let entries: Vec<_> = std::fs::read_dir(&self.path)?
            .filter_map(|e| e.ok())
            .filter(|e| e.file_name() != ".git")
            .collect();

        let paths: Vec<PathBuf> = entries.iter().map(|e| self.path.join(e.file_name())).collect();

        // Single batch gitignore check instead of per-file subprocess
        let ignored = helix_vcs::are_ignored(&paths);

        let ret = entries
            .into_iter()
            .zip(ignored)
            .filter_map(|(entry, is_ignored)| {
                if !state.show_ignored && is_ignored {
                    return None;
                }
                let symlink_meta = entry.metadata().ok()?;
                let is_symlink = entry.file_type().ok().map_or(false, |ft| ft.is_symlink());
                let file_type = if symlink_meta.is_dir() {
                    FileType::Folder
                } else {
                    FileType::File
                };
                Some(FileInfo {
                    file_type,
                    path: self.path.join(entry.file_name()),
                    is_ignored,
                    is_symlink,
                })
            })
            .collect();
        Ok(ret)
    }

    fn name(&self) -> String {
        self.get_text().to_string()
    }

    fn path(&self) -> PathBuf {
        self.path.clone()
    }

    fn is_parent(&self) -> bool {
        matches!(self.file_type, FileType::Folder | FileType::Root)
    }

    fn dimmed(&self) -> bool {
        self.is_ignored
    }

    fn is_symlink(&self) -> bool {
        self.is_symlink
    }
}


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ClipboardOp {
    Cut,
    Copy,
}

#[derive(Clone, Debug)]
struct ExplorerClipboard {
    path: PathBuf,
    op: ClipboardOp,
}

#[derive(Clone, Debug, Default)]
struct State {
    focus: bool,
    open: bool,
    current_root: PathBuf,
    area_width: u16,
    area: Rect,
    show_ignored: bool,
}

impl State {
    fn new(focus: bool, current_root: PathBuf) -> Self {
        Self {
            focus,
            current_root,
            open: true,
            area_width: 0,
            area: Rect::default(),
            show_ignored: false,  // 默认隐藏被忽略的文件
        }
    }
}

pub struct Explorer {
    tree: TreeView<FileInfo>,
    show_help: bool,
    state: State,
    #[allow(clippy::type_complexity)]
    on_next_key: Option<Box<dyn FnMut(&mut Context, &mut Self, &KeyEvent) -> EventResult>>,
    column_width: u16,
    show_ignored: bool,
    git_status: HashMap<PathBuf, FileChangeStatus>,
    git_status_rx: Option<Receiver<(PathBuf, FileChangeStatus)>>,
    clipboard: Option<ExplorerClipboard>,
    pending_select: Option<Box<dyn Component>>,
}

impl Explorer {
    pub fn new(cx: &mut Context) -> Result<Self> {
        Self::from_editor(cx.editor)
    }

    pub fn from_editor(editor: &Editor) -> Result<Self> {
        let current_root = std::env::current_dir()
            .unwrap_or_else(|_| "./".into())
            .canonicalize()?;
        let tree = Self::new_tree_view(current_root.clone())?;
        let mut explorer = Self {
            tree,
            show_help: true,
            state: State::new(true, current_root),
            on_next_key: None,
            column_width: editor.config().explorer.column_width as u16,
            show_ignored: false,
            git_status: HashMap::new(),
            git_status_rx: None,
            clipboard: None,
            pending_select: None,
        };
        explorer.refresh_git_status(editor);
        Ok(explorer)
    }

    fn new_tree_view(root: PathBuf) -> Result<TreeView<FileInfo>> {
        let root = FileInfo::root(root);
        Ok(TreeView::build_tree(root)?.with_enter_fn(Self::toggle_current))
    }

    fn refresh_git_status(&mut self, editor: &Editor) {
        self.git_status.clear();
        let cwd = self.state.current_root.clone();
        let (tx, rx) = std::sync::mpsc::channel();
        editor.diff_providers.clone().for_each_changed_file(cwd, move |change| {
            match change {
                Ok(change) => {
                    let status = change.status();
                    let path = change.path().to_path_buf();
                    let _ = tx.send((path, status));
                    true
                }
                Err(_) => true,
            }
        });
        self.git_status_rx = Some(rx);
    }

    fn drain_git_status(&mut self) {
        let root = self.state.current_root.clone();
        if let Some(ref rx) = self.git_status_rx {
            while let Ok((path, status)) = rx.try_recv() {
                // Insert file status
                self.git_status.insert(path.clone(), status);
                // Propagate to parent directories (stop at explorer root)
                let mut parent = path.parent().map(|p| p.to_path_buf());
                while let Some(ref dir) = parent {
                    if !dir.starts_with(&root) {
                        break;
                    }
                    let entry = self.git_status.entry(dir.clone());
                    use std::collections::hash_map::Entry;
                    match entry {
                        Entry::Occupied(mut e) => {
                            if Self::status_priority(status) > Self::status_priority(*e.get()) {
                                e.insert(status);
                            }
                        }
                        Entry::Vacant(e) => {
                            e.insert(status);
                        }
                    }
                    parent = dir.parent().map(|p| p.to_path_buf());
                }
            }
        }
    }

    fn status_priority(status: FileChangeStatus) -> u8 {
        match status {
            FileChangeStatus::Untracked => 1,
            FileChangeStatus::Modified => 2,
            FileChangeStatus::Renamed => 3,
            FileChangeStatus::Deleted => 4,
            FileChangeStatus::Conflict => 5,
        }
    }

    /// Change the explorer root to a new directory, rebuilding the tree.
    fn change_root(&mut self, new_root: PathBuf) -> Result<()> {
        let tree = Self::new_tree_view(new_root.clone())?;
        self.tree = tree;
        self.state.current_root = new_root;
        Ok(())
    }

    pub fn reveal_file(&mut self, path: PathBuf) -> Result<()> {
        let current_root = self.state.current_root.canonicalize()?;
        let current_path = path.canonicalize()?;

        if current_path.strip_prefix(&current_root).is_err() {
            // File is outside the current root — switch root to its parent
            let new_root = current_path
                .parent()
                .ok_or_else(|| anyhow::anyhow!("Cannot determine parent of '{}'", current_path.display()))?
                .to_path_buf();
            self.change_root(new_root)?;
        }

        let current_root = self.state.current_root.canonicalize()?;
        let stripped = current_path.strip_prefix(&current_root)?;
        let segments: Vec<_> = stripped
            .components()
            .map(|c| c.as_os_str().to_string_lossy().to_string())
            .collect();
        self.tree.reveal_item(segments)?;
        Ok(())
    }

    pub fn reveal_current_file(&mut self, cx: &mut Context) -> Result<()> {
        self.focus();
        let current_document_path = doc!(cx.editor).path().cloned();
        match current_document_path {
            None => Ok(()),
            Some(current_path) => self.reveal_file(current_path),
        }
    }

    /// Silently sync the tree selection to match the currently active document.
    /// Does not change explorer focus/open state. If the file is outside the
    /// explorer root, the explorer root is changed to the file's parent directory.
    pub fn sync_with_current_doc(&mut self, editor: &Editor) {
        let view_id = editor.tree.focus;
        let doc_id = editor.tree.get(view_id).doc;
        if let Some(doc) = editor.document(doc_id) {
            if let Some(path) = doc.path().cloned() {
                let root_changed = !path
                    .canonicalize()
                    .ok()
                    .and_then(|p| p.strip_prefix(self.state.current_root.canonicalize().ok()?).ok().map(|_| ()))
                    .is_some();
                let _ = self.reveal_file(path);
                if root_changed {
                    self.refresh_git_status(editor);
                }
            }
        }
    }

    pub fn focus(&mut self) {
        self.state.focus = true;
        self.state.open = true;
    }

    fn unfocus(&mut self) {
        self.state.focus = false;
        self.clipboard = None;
    }

    pub fn close(&mut self) {
        self.state.focus = false;
        self.state.open = false;
    }

    pub fn is_open(&self) -> bool {
        self.state.open
    }

    pub fn is_focus(&self) -> bool {
        self.state.focus
    }

    fn new_create_file_or_folder_prompt(&mut self, cx: &mut Context) -> Result<()> {
        let folder_path = self.nearest_folder()?;
        let root = self.state.current_root.clone();
        let title = format!(
            "New file or folder (ends with '{}'):",
            std::path::MAIN_SEPARATOR
        );
        // Pre-fill only the relative path within the workspace
        let rel_folder = folder_path
            .strip_prefix(&root)
            .unwrap_or(&folder_path);
        let initial = if rel_folder == Path::new("") {
            String::new()
        } else {
            format!("{}/", rel_folder.display())
        };
        let prompt = Prompt::new(
            "".into(),
            None,
            ui::completers::none,
            move |cx, line, event| {
                if event != PromptEvent::Validate {
                    return;
                }
                // Resolve relative to workspace root
                let path = helix_stdx::path::normalize(root.join(line));
                // Ensure the path stays within the workspace
                if !path.starts_with(&root) {
                    cx.editor
                        .set_error("Cannot create files outside the workspace");
                    return;
                }
                let result = if line.ends_with(std::path::MAIN_SEPARATOR) {
                    std::fs::create_dir_all(&path).map(|_| path.clone())
                } else {
                    if let Some(parent) = path.parent() {
                        if let Err(e) = std::fs::create_dir_all(parent) {
                            cx.editor.set_error(format!("Failed to create: {}", e));
                            return;
                        }
                    }
                    std::fs::OpenOptions::new()
                        .create_new(true)
                        .write(true)
                        .open(&path)
                        .map(|_| path.clone())
                };
                match result {
                    Ok(p) => {
                        cx.editor.set_status(format!("Created '{}'", p.display()));
                        cx.editor.explorer_needs_refresh = true;
                    }
                    Err(e) => cx.editor.set_error(format!("Failed to create: {}", e)),
                }
            },
        )
        .with_line(initial, cx.editor);
        self.pending_select = Some(Box::new(ui::InputDialog::new(title, prompt)));
        Ok(())
    }

    fn nearest_folder(&self) -> Result<PathBuf> {
        let current = self.tree.current()?.item();
        if current.is_parent() {
            Ok(current.path.to_path_buf())
        } else {
            let parent_path = current.path.parent().ok_or_else(|| {
                anyhow::anyhow!(format!(
                    "Unable to get parent path of '{}'",
                    current.path.to_string_lossy()
                ))
            })?;
            Ok(parent_path.to_path_buf())
        }
    }

    fn cut_current(&mut self) -> Result<()> {
        let item = self.tree.current()?.item();
        ensure!(item.file_type != FileType::Root, "Cannot cut root");
        self.clipboard = Some(ExplorerClipboard {
            path: item.path.clone(),
            op: ClipboardOp::Cut,
        });
        Ok(())
    }

    fn copy_current(&mut self) -> Result<()> {
        let item = self.tree.current()?.item();
        ensure!(item.file_type != FileType::Root, "Cannot copy root");
        self.clipboard = Some(ExplorerClipboard {
            path: item.path.clone(),
            op: ClipboardOp::Copy,
        });
        Ok(())
    }

    fn paste(&mut self, cx: &mut Context) -> Result<()> {
        let clipboard = self.clipboard.take().ok_or_else(|| {
            anyhow::anyhow!("Nothing in clipboard")
        })?;
        ensure!(clipboard.path.exists(), "Source no longer exists: {}", clipboard.path.display());
        let dest_folder = self.nearest_folder()?;
        let file_name = clipboard.path.file_name().ok_or_else(|| {
            anyhow::anyhow!("Cannot get file name from clipboard path")
        })?;
        let dest = dest_folder.join(file_name);
        let dest = dedup_path(dest);

        match clipboard.op {
            ClipboardOp::Cut => {
                close_documents(clipboard.path.clone(), cx)?;
                std::fs::rename(&clipboard.path, &dest)?;
            }
            ClipboardOp::Copy => {
                if clipboard.path.is_dir() {
                    copy_dir_all(&clipboard.path, &dest)?;
                } else {
                    std::fs::copy(&clipboard.path, &dest)?;
                }
            }
        }

        self.tree.refresh()?;
        self.reveal_file(dest)
    }

    fn new_remove_prompt(&mut self) -> Result<()> {
        let item = self.tree.current()?.item();
        match item.file_type {
            FileType::Folder => self.new_remove_folder_prompt(),
            FileType::File => self.new_remove_file_prompt(),
            FileType::Root => bail!("Root is not removable"),
        }
    }

    fn new_rename_prompt(&mut self, cx: &mut Context) -> Result<()> {
        let item = self.tree.current_item()?;
        let path = item.path.clone();
        let rel = self.relative_display_path(&path);
        let root = self.state.current_root.clone();
        let title = "Rename";
        let old_path = path.clone();
        let prompt = Prompt::new(
            "".into(),
            None,
            ui::completers::none,
            move |cx, line, event| {
                if event != PromptEvent::Validate {
                    return;
                }
                let rel_path = PathBuf::from(line);
                if !rel_path.is_relative() {
                    cx.editor.set_error("Path must be relative to the working directory");
                    return;
                }
                let new_path = root.join(&rel_path);
                let new_path = helix_stdx::path::normalize(new_path);
                if !new_path.starts_with(&root) {
                    cx.editor.set_error("Cannot rename outside the working directory");
                    return;
                }
                // Close documents at the old path
                let ids: Vec<_> = cx.editor.documents.iter()
                    .filter_map(|(id, doc)| {
                        doc.path().filter(|p| p.starts_with(&old_path)).map(|_| *id)
                    })
                    .collect();
                for id in ids {
                    let _ = cx.editor.close_document(id, true);
                }
                // Create parent directories if needed
                if let Some(parent) = new_path.parent() {
                    if let Err(e) = std::fs::create_dir_all(parent) {
                        cx.editor.set_error(format!("Failed to rename: {}", e));
                        return;
                    }
                }
                match std::fs::rename(&old_path, &new_path) {
                    Ok(()) => {
                        cx.editor.set_status(format!(
                            "Renamed '{}' -> '{}'",
                            old_path.display(),
                            new_path.display()
                        ));
                        cx.editor.explorer_needs_refresh = true;
                    }
                    Err(e) => cx.editor.set_error(format!("Failed to rename: {}", e)),
                }
            },
        )
        .with_line(rel, cx.editor);
        self.pending_select = Some(Box::new(ui::InputDialog::new(title, prompt)));
        Ok(())
    }

    fn new_remove_file_prompt(&mut self) -> Result<()> {
        let item = self.tree.current_item()?;
        ensure!(
            item.path.is_file(),
            "The path '{}' is not a file",
            item.path.to_string_lossy()
        );
        let path = item.path.clone();
        let rel = self.relative_display_path(&path);
        self.pending_select = Some(Box::new(Self::make_delete_confirm("Delete", &rel, path)));
        Ok(())
    }

    fn new_remove_folder_prompt(&mut self) -> Result<()> {
        let item = self.tree.current_item()?;
        ensure!(
            item.path.is_dir(),
            "The path '{}' is not a folder",
            item.path.to_string_lossy()
        );
        let path = item.path.clone();
        let rel = self.relative_display_path(&path);
        self.pending_select = Some(Box::new(Self::make_delete_confirm("Delete", &rel, path)));
        Ok(())
    }

    fn relative_display_path(&self, path: &Path) -> String {
        path.strip_prefix(&self.state.current_root)
            .unwrap_or(path)
            .display()
            .to_string()
    }

    fn make_delete_confirm(title: &str, rel_path: &str, path: PathBuf) -> ui::ConfirmDialog {
        ui::ConfirmDialog::new(title, rel_path, move |cx: &mut Context| {
            let item_path = path.clone();
            let ids: Vec<_> = cx
                .editor
                .documents
                .iter()
                .filter_map(|(id, doc)| {
                    doc.path()
                        .filter(|p| p.starts_with(&item_path))
                        .map(|_| *id)
                })
                .collect();
            for id in ids {
                cx.editor.close_document(id, true).ok();
            }

            let result = if item_path.is_dir() {
                std::fs::remove_dir_all(&item_path)
            } else {
                std::fs::remove_file(&item_path)
            };
            match result {
                Ok(()) => {
                    cx.editor.set_status(format!(
                        "Deleted '{}'",
                        item_path.display()
                    ));
                    cx.editor.explorer_needs_refresh = true;
                }
                Err(err) => cx.editor.set_error(format!(
                    "Failed to delete '{}': {}",
                    item_path.display(),
                    err
                )),
            }
        })
    }

    fn toggle_current(item: &mut FileInfo, cx: &mut Context, state: &mut State) -> TreeOp {
        (|| -> Result<TreeOp> {
            if item.path == Path::new("") {
                return Ok(TreeOp::Noop);
            }
            let meta = std::fs::metadata(&item.path)?;
            if meta.is_file() {
                cx.editor.open(&item.path, Action::Replace)?;
                state.focus = false;
                return Ok(TreeOp::Noop);
            }

            if item.path.is_dir() {
                return Ok(TreeOp::GetChildsAndInsert);
            }

            Err(anyhow::anyhow!("Unknown file type: {:?}", meta.file_type()))
        })()
        .unwrap_or_else(|err| {
            cx.editor.set_error(format!("{err}"));
            TreeOp::Noop
        })
    }

    fn open_split(item: &mut FileInfo, cx: &mut Context, state: &mut State) -> TreeOp {
        (|| -> Result<TreeOp> {
            if item.path == Path::new("") {
                return Ok(TreeOp::Noop);
            }
            let meta = std::fs::metadata(&item.path)?;
            if meta.is_file() {
                // 使用 VerticalSplit 在右侧打开新窗口
                cx.editor.open(&item.path, Action::VerticalSplit)?;
                state.focus = false;
                return Ok(TreeOp::Noop);
            }

            if item.path.is_dir() {
                return Ok(TreeOp::GetChildsAndInsert);
            }

            Err(anyhow::anyhow!("Unknown file type: {:?}", meta.file_type()))
        })()
        .unwrap_or_else(|err| {
            cx.editor.set_error(format!("{err}"));
            TreeOp::Noop
        })
    }

    fn open_file_picker(&mut self, cx: &mut Context) {
        let root = self.state.current_root.clone();
        self.state.focus = false;
        cx.jobs.callback(async move {
            let call = crate::job::Callback::EditorCompositor(Box::new(
                move |editor: &mut Editor, compositor: &mut crate::compositor::Compositor| {
                    let picker = ui::file_picker(editor, root);
                    compositor.push(Box::new(ui::overlay::overlaid(picker)));
                },
            ));
            Ok(call)
        });
    }

    /// 在右侧分屏打开当前选中的文件
    fn open_selected_split(&mut self, cx: &mut Context) -> Result<()> {
        let current_path = self.tree.current_item()?.path.clone();
        if current_path.is_file() {
            cx.editor.open(&current_path, Action::VerticalSplit)?;
            self.state.focus = false;
            self.show_help = false;
        } else if current_path.is_dir() {
            // 如果是目录，展开/折叠目录
            let enter_event = Event::Key(KeyEvent {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::NONE,
            });
            self.tree
                .handle_key_event(&enter_event, cx, &mut self.state);
        }
        Ok(())
    }

    fn render_tree(
        &mut self,
        area: Rect,
        prompt_area: Rect,
        surface: &mut Surface,
        cx: &mut Context,
    ) {
        self.tree.ensure_root_opened();
        // Refresh tree if signaled by a modal dialog callback (delete/rename/create)
        if cx.editor.explorer_needs_refresh {
            cx.editor.explorer_needs_refresh = false;
            let _ = self.tree.refresh();
        }
        // Also refresh if the current item was deleted externally
        if let Ok(item) = self.tree.current_item() {
            if !item.path.exists() {
                let _ = self.tree.refresh();
            }
        }
        self.drain_git_status();
        let clipboard_info = self.clipboard.as_ref().map(|cb| {
            let label = match cb.op {
                ClipboardOp::Cut => "cut",
                ClipboardOp::Copy => "copy",
            };
            (cb.path.as_path(), label)
        });
        self.tree
            .render_with_git_status(area, prompt_area, surface, cx, &self.git_status, clipboard_info);
    }

    fn render_embed(
        &mut self,
        area: Rect,
        surface: &mut Surface,
        cx: &mut Context,
        position: &ExplorerPosition,
    ) {
        if !self.state.open {
            return;
        }
        let width = area.width.min(self.column_width + 2);

        self.state.area_width = area.width;

        let commandline = 0;
        let side_area = match position {
            ExplorerPosition::Left => Rect { width, ..area },
            ExplorerPosition::Right => Rect {
                x: area.width - width,
                width,
                ..area
            },
        }
        .clip_bottom(commandline);

        self.state.area = side_area;

        let background = cx.editor.theme.get("ui.background");
        surface.clear_with(side_area, background);

        // Prompt should render in the command line (bottom of terminal),
        // which is 2 rows below the clipped area (statusline + commandline).
        let prompt_area = Rect::new(area.x, area.y + area.height + 1, area.width, 1);

        let split_style = cx.editor.theme.get("ui.window");
        let border = match position {
            ExplorerPosition::Left => Borders::RIGHT,
            ExplorerPosition::Right => Borders::LEFT,
        };

        let list_area =
            render_block(side_area, surface, border, split_style);

        self.render_tree(list_area, prompt_area, surface, cx);

        if self.is_focus() && self.show_help {
            cx.editor.autoinfo = Some(Self::help_info());
        }
    }

    fn help_info() -> Info {
        Info::new(
            "Explorer",
            &[
                ("o, Enter", "Open/Close"),
                ("j/k", "Down/Up"),
                ("h/l", "Parent/Expand"),
                ("/", "Search"),
                ("n/N", "Next/Prev match"),
                ("a", "Add file/folder"),
                ("r", "Rename"),
                ("d", "Delete"),
                ("x", "Cut"),
                ("c", "Copy"),
                ("p", "Paste"),
                ("s", "Open in split"),
                ("R", "Refresh"),
                (".", "Toggle ignored"),
                ("C", "Collapse to parent"),
                ("B", "Change root to parent"),
                ("]", "Change root to current"),
                ("[", "Go to previous root"),
                ("+/-", "Resize"),
                ("q", "Close"),
                ("?", "Toggle help"),
            ],
        )
    }


    fn toggle_help(&mut self) {
        self.show_help = !self.show_help
    }

    fn toggle_show_ignored(&mut self) {
        self.show_ignored = !self.show_ignored;
        self.state.show_ignored = self.show_ignored;
        // 更新 TreeView 的 params
        self.tree.set_params(self.state.clone());
        // 刷新树以应用新的过滤设置
        if let Err(err) = self.tree.refresh() {
            log::warn!("Failed to refresh tree: {}", err);
        }
    }


    pub fn is_opened(&self) -> bool {
        self.state.open
    }

    pub fn column_width(&self) -> u16 {
        self.column_width
    }

    fn increase_size(&mut self) {
        const EDITOR_MIN_WIDTH: u16 = 10;
        self.column_width = std::cmp::min(
            self.state.area_width.saturating_sub(EDITOR_MIN_WIDTH),
            self.column_width.saturating_add(1),
        )
    }

    fn decrease_size(&mut self) {
        self.column_width = self.column_width.saturating_sub(1)
    }

}

fn close_documents(current_item_path: PathBuf, cx: &mut Context) -> Result<()> {
    let ids = cx
        .editor
        .documents
        .iter()
        .filter_map(|(id, doc)| {
            if doc.path()?.starts_with(&current_item_path) {
                Some(*id)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    for id in ids {
        cx.editor.close_document(id, true)?;
    }
    Ok(())
}

/// If `path` already exists, append a numeric suffix to produce a unique name.
/// e.g. `foo.txt` → `foo_1.txt` → `foo_2.txt`, `dir` → `dir_1` → `dir_2`
fn dedup_path(path: PathBuf) -> PathBuf {
    if !path.exists() {
        return path;
    }
    let stem = path
        .file_stem()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string();
    let ext = path.extension().map(|e| e.to_string_lossy().to_string());
    let parent = path.parent().unwrap_or(Path::new(""));
    for i in 1u32.. {
        let new_name = match &ext {
            Some(e) => format!("{}_{}.{}", stem, i, e),
            None => format!("{}_{}", stem, i),
        };
        let candidate = parent.join(new_name);
        if !candidate.exists() {
            return candidate;
        }
    }
    path
}

fn copy_dir_all(src: &Path, dst: &Path) -> Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        let dest_path = dst.join(entry.file_name());
        if ty.is_dir() {
            copy_dir_all(&entry.path(), &dest_path)?;
        } else {
            std::fs::copy(entry.path(), &dest_path)?;
        }
    }
    Ok(())
}

impl Component for Explorer {
    /// Process input events, return true if handled.
    fn handle_event(&mut self, event: &Event, cx: &mut Context) -> EventResult {
        if self.tree.prompting() {
            return self.tree.handle_key_event(event, cx, &mut self.state);
        }

        let key_event = match event {
            Event::Key(event) => event,
            Event::Mouse(event) => {
                let config = &cx.editor.config().explorer;
                let is_in_area = event.column > self.state.area.x
                    && match config.position {
                        ExplorerPosition::Left => event.column < self.state.area.width,
                        ExplorerPosition::Right => {
                            event.column < self.state.area.width + self.state.area.x
                        }
                    };
                if self.is_opened() && is_in_area {
                    let result = self.tree.handle_mouse_event(event, cx, &mut self.state);
                    if let EventResult::Consumed(_) = result {
                        self.focus();
                    };
                    return result;
                }
                self.unfocus();
                return EventResult::Ignored(None);
            }
            Event::Resize(..) => return EventResult::Consumed(None),
            _ => return EventResult::Ignored(None),
        };
        if !self.is_focus() {
            return EventResult::Ignored(None);
        }
        if let Some(mut on_next_key) = self.on_next_key.take() {
            return on_next_key(cx, self, key_event);
        }

        if let key!(':') = key_event {
            return EventResult::Ignored(None);
        }

        (|| -> Result<()> {
            match key_event {
                key!(Esc) => {
                    self.clipboard = None;
                    self.unfocus();
                }
                key!('q') | ctrl!('b') => self.close(),
                key!('?') => self.toggle_help(),
                key!('a') => self.new_create_file_or_folder_prompt(cx)?,
                key!('d') => self.new_remove_prompt()?,
                key!('r') => self.new_rename_prompt(cx)?,
                key!('x') => self.cut_current()?,
                key!('c') => self.copy_current()?,
                key!('p') => self.paste(cx)?,
                key!('-') | key!('_') => self.decrease_size(),
                key!('+') | key!('=') => self.increase_size(),
                shift!('C') => self.tree.collapse_to_parent()?,
                key!('.') => self.toggle_show_ignored(),
                key!('s') => self.open_selected_split(cx)?,
                key!(' ') => {
                    self.on_next_key = Some(Box::new(|cx, explorer, key_event| {
                        match key_event {
                            key!('f') => explorer.open_file_picker(cx),
                            _ => {}
                        }
                        EventResult::Consumed(None)
                    }));
                }
                _ => {
                    self.tree
                        .handle_key_event(&Event::Key(*key_event), cx, &mut self.state);
                }
            };

            // 如果打开文件后焦点丢失，同步关闭帮助面板
            if !self.state.focus {
                self.show_help = false;
            }

            Ok(())
        })()
        .unwrap_or_else(|err| cx.editor.set_error(format!("{err}")));

        if let Some(select) = self.pending_select.take() {
            self.show_help = false;
            cx.editor.autoinfo = None;
            let callback: crate::compositor::Callback = Box::new(
                move |compositor: &mut crate::compositor::Compositor, _cx: &mut crate::compositor::Context| {
                    compositor.push(select);
                },
            );
            return EventResult::Consumed(Some(callback));
        }

        EventResult::Consumed(None)
    }

    fn render(&mut self, area: Rect, surface: &mut Surface, cx: &mut Context) {
        if area.width < 10 || area.height < 5 {
            cx.editor.set_error("explorer render area is too small");
            return;
        }
        let config = &cx.editor.config().explorer;
        let position = config.position;
        self.render_embed(area, surface, cx, &position);
    }

    fn cursor(&self, area: Rect, editor: &Editor) -> (Option<Position>, CursorKind) {
        if let Some(prompt) = self.tree.prompt() {
            let (x, y) = (area.x, area.y + area.height.saturating_sub(1));
            prompt.cursor(Rect::new(x, y, area.width, 1), editor)
        } else {
            (None, CursorKind::Hidden)
        }
    }
}

fn render_block(area: Rect, surface: &mut Surface, borders: Borders, split_style: Style) -> Rect {
    let block = Block::default().borders(borders).border_style(split_style);
    let inner = block.inner(area);
    block.render(area, surface);
    inner
}
