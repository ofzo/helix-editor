use crate::{
    compositor::{self, Component, Context, Event, EventResult},
    ctrl, key,
    ui::{
        document::{render_document, LinePos, TextRenderer},
        text_decorations::DecorationManager,
        EditorView,
    },
};
use helix_core::{
    char_idx_at_visual_offset, chars::{categorize_char, CharCategory},
    text_annotations::TextAnnotations, Position, Uri,
};
use helix_lsp::{lsp, OffsetEncoding};
use helix_view::{
    align_view, Align,
    editor::Action,
    graphics::{CursorKind, Margin, Rect},
    view::ViewPosition,
    Document, Editor,
};
use std::{
    collections::HashMap,
    io::Read,
    path::{Path, PathBuf},
    sync::Arc,
};
use tui::{
    buffer::Buffer as Surface,
    widgets::{Block, Widget},
};

pub const ID: &str = "reference_finder";

const MAX_FILE_SIZE_FOR_PREVIEW: u64 = 10 * 1024 * 1024;

#[derive(Debug, Clone)]
pub struct RefEntry {
    pub line: usize,
    pub col: usize,
    pub text: String,
    pub range: lsp::Range,
    pub offset_encoding: OffsetEncoding,
}

#[derive(Debug)]
pub struct FileGroup {
    pub path: Arc<Path>,
    pub display_path: String,
    pub references: Vec<RefEntry>,
    pub is_expanded: bool,
}

#[derive(Debug, Clone)]
enum SelectedItem<'a> {
    Group(usize),
    Entry(usize, usize, &'a RefEntry),
}

enum CachedPreview {
    Document(Box<Document>),
    Binary,
    LargeFile,
    NotFound,
}

pub struct ReferenceFinder {
    groups: Vec<FileGroup>,
    selected: usize,
    total_visible: usize,
    scroll_offset: usize,
    preview_cache: HashMap<Arc<Path>, CachedPreview>,
    read_buffer: Vec<u8>,
    /// Offset from symbol start column to cursor column (in document chars)
    symbol_col_offset: usize,
    /// Document cursor position (char index) at creation time
    doc_cursor_pos: usize,
    /// Document ID at creation time
    doc_id: helix_view::DocumentId,
    /// Title shown in the tree panel border
    title: String,
}

impl ReferenceFinder {
    pub fn new(
        locations: Vec<(Uri, lsp::Range, OffsetEncoding)>,
        cwdir: PathBuf,
        editor: &Editor,
    ) -> Self {
        let mut groups_map: HashMap<PathBuf, Vec<RefEntry>> = HashMap::new();
        let mut order: Vec<PathBuf> = Vec::new();

        for (uri, range, offset_encoding) in locations {
            let Some(path) = uri.as_path() else { continue };
            let path_buf = path.to_path_buf();

            let line = range.start.line as usize;
            let col = range.start.character as usize;
            let text = read_line_content(editor, path, line);

            let entry = RefEntry {
                line,
                col,
                text,
                range,
                offset_encoding,
            };

            if !groups_map.contains_key(&path_buf) {
                order.push(path_buf.clone());
            }
            groups_map.entry(path_buf).or_default().push(entry);
        }

        let groups: Vec<FileGroup> = order
            .into_iter()
            .map(|path| {
                let mut refs = groups_map.remove(&path).unwrap();
                refs.sort_by_key(|r| (r.line, r.col));

                let display_path = path
                    .strip_prefix(&cwdir)
                    .unwrap_or(&path)
                    .to_string_lossy()
                    .into_owned();

                FileGroup {
                    path: Arc::from(path.as_path()),
                    display_path,
                    references: refs,
                    is_expanded: true,
                }
            })
            .collect();

        let total_visible = calculate_visible(&groups);

        // Compute offset from symbol start to cursor position, and record doc cursor
        let (symbol_col_offset, doc_cursor_pos, doc_id) = {
            let (view, doc) = helix_view::current_ref!(editor);
            let text = doc.text().slice(..);
            let cursor = doc.selection(view.id).primary().cursor(text);
            let line = text.char_to_line(cursor);
            let line_start = text.line_to_char(line);
            let cursor_col = cursor - line_start;

            let mut word_start = cursor_col;
            for i in (0..cursor_col).rev() {
                let ch = text.char(line_start + i);
                if categorize_char(ch) != CharCategory::Word {
                    break;
                }
                word_start = i;
            }
            (cursor_col - word_start, cursor, doc.id())
        };

        ReferenceFinder {
            groups,
            selected: 0,
            total_visible,
            scroll_offset: 0,
            preview_cache: HashMap::new(),
            read_buffer: Vec::new(),
            symbol_col_offset,
            doc_cursor_pos,
            doc_id,
            title: "References".to_string(),
        }
    }

    pub fn with_title(mut self, title: impl Into<String>) -> Self {
        self.title = title.into();
        self
    }

    fn selected_item(&self) -> Option<SelectedItem<'_>> {
        let mut idx = 0;
        for (gi, group) in self.groups.iter().enumerate() {
            if idx == self.selected {
                return Some(SelectedItem::Group(gi));
            }
            idx += 1;
            if group.is_expanded {
                for (ri, entry) in group.references.iter().enumerate() {
                    if idx == self.selected {
                        return Some(SelectedItem::Entry(gi, ri, entry));
                    }
                    idx += 1;
                }
            }
        }
        None
    }

    fn selected_path_and_range(&self) -> Option<(&Path, Option<(usize, usize)>)> {
        match self.selected_item()? {
            SelectedItem::Group(gi) => {
                let group = &self.groups[gi];
                let range = group
                    .references
                    .first()
                    .map(|r| (r.line, r.range.end.line as usize));
                Some((&group.path, range))
            }
            SelectedItem::Entry(gi, _ri, entry) => {
                let group = &self.groups[gi];
                Some((&group.path, Some((entry.line, entry.range.end.line as usize))))
            }
        }
    }

    fn move_up(&mut self) {
        if self.selected > 0 {
            self.selected -= 1;
        }
    }

    fn move_down(&mut self) {
        if self.selected + 1 < self.total_visible {
            self.selected += 1;
        }
    }

    fn move_half_page_up(&mut self, height: usize) {
        self.selected = self.selected.saturating_sub(height / 2);
    }

    fn move_half_page_down(&mut self, height: usize) {
        self.selected = (self.selected + height / 2).min(self.total_visible.saturating_sub(1));
    }

    fn go_to_first(&mut self) {
        self.selected = 0;
    }

    fn go_to_last(&mut self) {
        self.selected = self.total_visible.saturating_sub(1);
    }

    fn prev_group(&mut self) {
        let mut idx = 0;
        let mut current_group = 0;
        for (gi, group) in self.groups.iter().enumerate() {
            if idx >= self.selected {
                current_group = gi;
                break;
            }
            idx += 1;
            if group.is_expanded {
                if idx + group.references.len() > self.selected {
                    current_group = gi;
                    break;
                }
                idx += group.references.len();
            }
            current_group = gi + 1;
        }

        if current_group > 0 {
            let target_group = current_group - 1;
            let mut target_idx = 0;
            for (gi, group) in self.groups.iter().enumerate() {
                if gi == target_group {
                    break;
                }
                target_idx += 1;
                if group.is_expanded {
                    target_idx += group.references.len();
                }
            }
            self.selected = target_idx;
        }
    }

    fn next_group(&mut self) {
        let mut idx = 0;
        let mut found_current = false;
        for group in &self.groups {
            if found_current {
                self.selected = idx;
                return;
            }
            let group_end = idx + if group.is_expanded { group.references.len() } else { 0 };
            if idx <= self.selected && self.selected <= group_end {
                found_current = true;
            }
            idx += 1;
            if group.is_expanded {
                idx += group.references.len();
            }
        }
    }

    fn toggle_expand(&mut self) {
        if let Some(SelectedItem::Group(gi)) = self.selected_item() {
            self.groups[gi].is_expanded = !self.groups[gi].is_expanded;
            self.total_visible = calculate_visible(&self.groups);
            if self.selected >= self.total_visible {
                self.selected = self.total_visible.saturating_sub(1);
            }
        }
    }

    fn expand_selected(&mut self) {
        if let Some(SelectedItem::Group(gi)) = self.selected_item() {
            if !self.groups[gi].is_expanded {
                self.groups[gi].is_expanded = true;
                self.total_visible = calculate_visible(&self.groups);
            }
        }
    }

    fn collapse_selected(&mut self) {
        match self.selected_item() {
            Some(SelectedItem::Group(gi)) => {
                if self.groups[gi].is_expanded {
                    self.groups[gi].is_expanded = false;
                    self.total_visible = calculate_visible(&self.groups);
                }
            }
            Some(SelectedItem::Entry(gi, _, _)) => {
                self.groups[gi].is_expanded = false;
                self.total_visible = calculate_visible(&self.groups);
                // Move selection to the group header
                let mut idx = 0;
                for i in 0..gi {
                    idx += 1;
                    if self.groups[i].is_expanded {
                        idx += self.groups[i].references.len();
                    }
                }
                self.selected = idx;
            }
            _ => {}
        }
    }

    fn make_jump_callback(
        &self,
        gi: usize,
        ri: usize,
        action: Action,
    ) -> Option<EventResult> {
        let group = &self.groups[gi];
        let entry = &group.references[ri];
        let path = group.path.clone();
        let range = entry.range;
        let offset_encoding = entry.offset_encoding;
        Some(EventResult::Consumed(Some(Box::new(
            move |compositor: &mut compositor::Compositor, cx: &mut compositor::Context| {
                compositor.pop();
                jump_to(cx.editor, &path, range, offset_encoding, action);
            },
        ))))
    }

    fn get_preview<'finder, 'editor>(
        &'finder mut self,
        editor: &'editor Editor,
    ) -> Option<(PreviewDoc<'finder, 'editor>, Option<(usize, usize)>)> {
        let (path, range) = self.selected_path_and_range()?;

        if let Some(doc) = editor.document_by_path(path) {
            return Some((PreviewDoc::Editor(doc), range));
        }

        if self.preview_cache.contains_key(path) {
            let preview = self.preview_cache.get(path).unwrap();
            return Some((PreviewDoc::Cached(preview), range));
        }

        let path: Arc<Path> = path.into();
        let preview = std::fs::metadata(&path)
            .and_then(|metadata| {
                if !metadata.is_file() {
                    return Ok(CachedPreview::NotFound);
                }
                if metadata.len() > MAX_FILE_SIZE_FOR_PREVIEW {
                    return Ok(CachedPreview::LargeFile);
                }
                let content_type = std::fs::File::open(&path).and_then(|file| {
                    let n = file.take(1024).read_to_end(&mut self.read_buffer)?;
                    let ct = content_inspector::inspect(&self.read_buffer[..n]);
                    self.read_buffer.clear();
                    Ok(ct)
                })?;
                if content_type.is_binary() {
                    return Ok(CachedPreview::Binary);
                }
                let mut doc = Document::open(
                    &path,
                    None,
                    false,
                    editor.config.clone(),
                    editor.syn_loader.clone(),
                    &editor.diff_providers,
                )
                .or(Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "Cannot open document",
                )))?;
                let loader = editor.syn_loader.load();
                if let Some(language_config) = doc.detect_language_config(&loader) {
                    let language = language_config.language();
                    doc.language = Some(language_config);
                    if let Ok(syntax) =
                        helix_core::Syntax::new(doc.text().slice(..), language, &loader)
                    {
                        doc.syntax = Some(syntax);
                    }
                }
                Ok(CachedPreview::Document(Box::new(doc)))
            })
            .unwrap_or(CachedPreview::NotFound);
        self.preview_cache.insert(path.clone(), preview);
        Some((PreviewDoc::Cached(&self.preview_cache[&path]), range))
    }

    fn render_tree(&mut self, area: Rect, surface: &mut Surface, cx: &mut Context) {
        let background = cx.editor.theme.get("ui.background");
        let text_style = cx.editor.theme.get("ui.text");
        let dir_style = cx.editor.theme.get("ui.text.directory");
        let selected_style = cx.editor.theme.get("ui.menu.selected");
        let line_nr_style = cx
            .editor
            .theme
            .try_get("ui.linenr")
            .unwrap_or(text_style);

        surface.clear_with(area, background);

        let total_refs: usize = self.groups.iter().map(|g| g.references.len()).sum();
        let title = format!(" {} ({}) ", self.title, total_refs);
        let block = Block::bordered().title(title.as_str());
        let inner = block.inner(area);
        block.render(area, surface);

        let inner = inner.inner(Margin::horizontal(1));

        let height = inner.height as usize;
        if height == 0 {
            return;
        }
        if self.selected < self.scroll_offset {
            self.scroll_offset = self.selected;
        }
        if self.selected >= self.scroll_offset + height {
            self.scroll_offset = self.selected - height + 1;
        }

        let mut flat_idx: usize = 0;
        let mut y_offset: usize = 0;

        for group in &self.groups {
            if flat_idx >= self.scroll_offset && y_offset < height {
                let y = inner.y + y_offset as u16;
                let is_selected = flat_idx == self.selected;
                let style = if is_selected { selected_style } else { dir_style };

                if is_selected {
                    surface.clear_with(Rect::new(inner.x, y, inner.width, 1), selected_style);
                }

                let chevron = if group.is_expanded { " " } else { " " };
                let header = format!("{}{} [{}]", chevron, &group.display_path, group.references.len());
                surface.set_stringn(inner.x, y, &header, inner.width as usize, style);
                y_offset += 1;
            } else if flat_idx >= self.scroll_offset {
                y_offset += 1;
            }
            flat_idx += 1;

            if group.is_expanded {
                let last_idx = group.references.len().saturating_sub(1);
                for (ri, entry) in group.references.iter().enumerate() {
                    if flat_idx >= self.scroll_offset && y_offset < height {
                        let y = inner.y + y_offset as u16;
                        let is_selected = flat_idx == self.selected;

                        if is_selected {
                            surface.clear_with(Rect::new(inner.x, y, inner.width, 1), selected_style);
                        }

                        let connector = if ri == last_idx { "└ " } else { "├ " };
                        let line_nr = format!("{}:", entry.line + 1);
                        let style = if is_selected { selected_style } else { text_style };
                        let nr_style = if is_selected { selected_style } else { line_nr_style };

                        let mut x = inner.x;
                        surface.set_stringn(x, y, "  ", 2, style);
                        x += 2;
                        surface.set_stringn(x, y, connector, 2, style);
                        x += 2;
                        let nr_width = line_nr.len();
                        surface.set_stringn(x, y, &line_nr, nr_width, nr_style);
                        x += nr_width as u16;
                        surface.set_stringn(x, y, " ", 1, style);
                        x += 1;
                        let remaining = inner.width.saturating_sub(x - inner.x) as usize;
                        surface.set_stringn(x, y, &entry.text, remaining, style);

                        y_offset += 1;
                    } else if flat_idx >= self.scroll_offset {
                        y_offset += 1;
                    }
                    flat_idx += 1;
                }
            }

            if y_offset >= height {
                break;
            }
        }
    }

    fn render_preview(&mut self, area: Rect, surface: &mut Surface, cx: &mut Context) {
        let background = cx.editor.theme.get("ui.background");
        let text_style = cx.editor.theme.get("ui.text");
        surface.clear_with(area, background);

        let block = Block::bordered();
        let inner = block.inner(area);
        let inner = inner.inner(Margin::horizontal(1));
        block.render(area, surface);

        let Some((preview, range)) = self.get_preview(cx.editor) else {
            let msg = "No preview available";
            let x = inner.x + inner.width.saturating_sub(msg.len() as u16) / 2;
            let y = inner.y + inner.height / 2;
            surface.set_stringn(x, y, msg, inner.width as usize, text_style);
            return;
        };

        let doc = match preview.document() {
            Some(doc)
                if range.is_none_or(|(start, end)| {
                    start <= end && end <= doc.text().len_lines()
                }) =>
            {
                doc
            }
            _ => {
                let msg = preview.placeholder();
                let x = inner.x + inner.width.saturating_sub(msg.len() as u16) / 2;
                let y = inner.y + inner.height / 2;
                surface.set_stringn(x, y, msg, inner.width as usize, text_style);
                return;
            }
        };

        let mut offset = ViewPosition::default();
        if let Some((start_line, end_line)) = range {
            let height = end_line - start_line;
            let text = doc.text().slice(..);
            let start = text.line_to_char(start_line);
            let middle = text.line_to_char(start_line + height / 2);
            if height < inner.height as usize {
                let text_fmt = doc.text_format(inner.width, None);
                let annotations = TextAnnotations::default();
                (offset.anchor, offset.vertical_offset) = char_idx_at_visual_offset(
                    text,
                    middle,
                    -(inner.height as isize / 2),
                    0,
                    &text_fmt,
                    &annotations,
                );
                if start < offset.anchor {
                    offset.anchor = start;
                    offset.vertical_offset = 0;
                }
            } else {
                offset.anchor = start;
            }
        }

        let loader = cx.editor.syn_loader.load();
        let config = cx.editor.config();

        let syntax_highlighter =
            EditorView::doc_syntax_highlighter(doc, offset.anchor, inner.height, &loader);
        let mut overlay_highlights = Vec::new();
        if doc
            .language_config()
            .and_then(|c| c.rainbow_brackets)
            .unwrap_or(config.rainbow_brackets)
        {
            if let Some(overlay) = EditorView::doc_rainbow_highlights(
                doc,
                offset.anchor,
                inner.height,
                &cx.editor.theme,
                &loader,
            ) {
                overlay_highlights.push(overlay);
            }
        }

        EditorView::doc_diagnostics_highlights_into(
            doc,
            &cx.editor.theme,
            &mut overlay_highlights,
        );

        let mut decorations = DecorationManager::default();

        if let Some((start, end)) = range {
            let style = cx
                .editor
                .theme
                .try_get("ui.highlight")
                .unwrap_or_else(|| cx.editor.theme.get("ui.selection"));
            let draw_highlight = move |renderer: &mut TextRenderer, pos: LinePos| {
                if (start..=end).contains(&pos.doc_line) {
                    let area = Rect::new(
                        renderer.viewport.x,
                        pos.visual_line,
                        renderer.viewport.width,
                        1,
                    );
                    renderer.set_style(area, style)
                }
            };
            decorations.add_decoration(draw_highlight);
        }

        render_document(
            surface,
            inner,
            doc,
            offset,
            &TextAnnotations::default(),
            syntax_highlighter,
            overlay_highlights,
            &cx.editor.theme,
            decorations,
            &[],
            None,
        );
    }
}

enum PreviewDoc<'finder, 'editor> {
    Cached(&'finder CachedPreview),
    Editor(&'editor Document),
}

impl PreviewDoc<'_, '_> {
    fn document(&self) -> Option<&Document> {
        match self {
            PreviewDoc::Editor(doc) => Some(doc),
            PreviewDoc::Cached(CachedPreview::Document(doc)) => Some(doc),
            _ => None,
        }
    }

    fn placeholder(&self) -> &str {
        match self {
            PreviewDoc::Editor(_) => "<Invalid file location>",
            PreviewDoc::Cached(preview) => match preview {
                CachedPreview::Document(_) => "<Invalid file location>",
                CachedPreview::Binary => "<Binary file>",
                CachedPreview::LargeFile => "<File too large to preview>",
                CachedPreview::NotFound => "<File not found>",
            },
        }
    }
}

impl Component for ReferenceFinder {
    fn handle_event(&mut self, event: &Event, ctx: &mut Context) -> EventResult {
        // Check if document cursor has moved since creation (e.g. from a previous unhandled key)
        {
            let (view, doc) = helix_view::current_ref!(ctx.editor);
            let cursor_moved = if doc.id() != self.doc_id {
                true
            } else {
                let text = doc.text().slice(..);
                let cursor = doc.selection(view.id).primary().cursor(text);
                cursor != self.doc_cursor_pos
            };
            if cursor_moved {
                return EventResult::Consumed(Some(Box::new(
                    |compositor: &mut compositor::Compositor, _cx: &mut compositor::Context| {
                        compositor.remove(ID);
                    },
                )));
            }
        }

        let Event::Key(key_event) = event else {
            return EventResult::Ignored(None);
        };

        let close_fn =
            EventResult::Consumed(Some(Box::new(|compositor: &mut compositor::Compositor, _cx: &mut compositor::Context| {
                compositor.pop();
            })));

        match key_event {
            key!(Esc) | ctrl!('c') => {
                return close_fn;
            }
            key!('j') | key!(Down) | ctrl!('n') => self.move_down(),
            key!('k') | key!(Up) | ctrl!('p') => self.move_up(),
            key!('h') | key!(Left) => self.collapse_selected(),
            key!('l') | key!(Right) => self.expand_selected(),
            key!(Enter) => {
                match self.selected_item() {
                    Some(SelectedItem::Group(_)) => self.toggle_expand(),
                    Some(SelectedItem::Entry(gi, ri, _)) => {
                        if let Some(result) = self.make_jump_callback(gi, ri, Action::Replace) {
                            return result;
                        }
                    }
                    None => {}
                }
            }
            ctrl!('s') => {
                if let Some(SelectedItem::Entry(gi, ri, _)) = self.selected_item() {
                    if let Some(result) = self.make_jump_callback(gi, ri, Action::HorizontalSplit) {
                        return result;
                    }
                }
            }
            ctrl!('v') => {
                if let Some(SelectedItem::Entry(gi, ri, _)) = self.selected_item() {
                    if let Some(result) = self.make_jump_callback(gi, ri, Action::VerticalSplit) {
                        return result;
                    }
                }
            }
            ctrl!('d') => self.move_half_page_down(20),
            ctrl!('u') => self.move_half_page_up(20),
            key!('g') => self.go_to_first(),
            key!('G') => self.go_to_last(),
            key!('[') => self.prev_group(),
            key!(']') => self.next_group(),
            _ => return EventResult::Ignored(None),
        }

        EventResult::Consumed(None)
    }

    fn render(&mut self, viewport: Rect, surface: &mut Surface, cx: &mut Context) {
        // If document cursor moved, hide (don't render). handle_event will clean up.
        {
            let (view, doc) = helix_view::current_ref!(cx.editor);
            let stale = if doc.id() != self.doc_id {
                true
            } else {
                let text = doc.text().slice(..);
                doc.selection(view.id).primary().cursor(text) != self.doc_cursor_pos
            };
            if stale {
                return;
            }
        }

        const MIN_HEIGHT: u16 = 16;
        // Minimum width for left-right layout (tree + preview side by side)
        const MIN_WIDTH_FOR_HORIZONTAL: u16 = 60;

        // Get current cursor screen position (adapts to sidebar toggle, viewport resize)
        let cursor_pos = cx.editor.cursor().0.unwrap_or_default();
        let mut cursor_row = cursor_pos.row as u16;
        let symbol_col = (cursor_pos.col as u16).saturating_sub(self.symbol_col_offset as u16);

        let mut space_below = viewport.height.saturating_sub(cursor_row + 1);
        let space_above = cursor_row;

        // If not enough vertical space in either direction, scroll document
        if space_below < MIN_HEIGHT && space_above < MIN_HEIGHT {
            let (view, doc) = helix_view::current!(cx.editor);
            align_view(doc, view, Align::Top);
            if let Some(new_pos) = cx.editor.cursor().0 {
                cursor_row = new_pos.row as u16;
                space_below = viewport.height.saturating_sub(cursor_row + 1);
            }
        }

        // Determine vertical placement
        let (y, panel_height) = if space_below >= MIN_HEIGHT {
            (cursor_row + 1, space_below)
        } else if space_above >= MIN_HEIGHT {
            (0, space_above)
        } else if space_below >= space_above {
            (cursor_row + 1, space_below)
        } else {
            (0, space_above)
        };

        let panel_width = viewport.width.saturating_sub(symbol_col);
        let area = Rect::new(symbol_col, y, panel_width, panel_height);

        // Choose layout based on available width
        if panel_width >= MIN_WIDTH_FOR_HORIZONTAL {
            // Left-right layout: tree (40%) | preview (60%)
            let tree_width = (area.width * 2 / 5).max(20).min(area.width);
            let tree_area = Rect::new(area.x, area.y, tree_width, area.height);
            let preview_area = Rect::new(
                area.x + tree_width,
                area.y,
                area.width.saturating_sub(tree_width),
                area.height,
            );
            self.render_tree(tree_area, surface, cx);
            if preview_area.width > 2 {
                self.render_preview(preview_area, surface, cx);
            }
        } else {
            // Top-bottom layout: tree (top 40%) / preview (bottom 60%)
            let tree_height = (area.height * 2 / 5).max(4).min(area.height);
            let tree_area = Rect::new(area.x, area.y, area.width, tree_height);
            let preview_height = area.height.saturating_sub(tree_height);
            let preview_area = Rect::new(area.x, area.y + tree_height, area.width, preview_height);
            self.render_tree(tree_area, surface, cx);
            if preview_area.height > 2 {
                self.render_preview(preview_area, surface, cx);
            }
        }
    }

    fn cursor(&self, _area: Rect, _editor: &Editor) -> (Option<Position>, CursorKind) {
        (None, CursorKind::Hidden)
    }

    fn required_size(&mut self, _viewport: (u16, u16)) -> Option<(u16, u16)> {
        None
    }

    fn id(&self) -> Option<&'static str> {
        Some(ID)
    }
}

fn calculate_visible(groups: &[FileGroup]) -> usize {
    groups.iter().fold(0, |acc, g| {
        acc + 1 + if g.is_expanded { g.references.len() } else { 0 }
    })
}

fn read_line_content(editor: &Editor, path: &Path, line: usize) -> String {
    if let Some(doc) = editor.document_by_path(path) {
        let text = doc.text();
        if line < text.len_lines() {
            return text.line(line).to_string().trim_end().to_string();
        }
    }

    if let Ok(content) = std::fs::read_to_string(path) {
        if let Some(line_text) = content.lines().nth(line) {
            return line_text.trim_end().to_string();
        }
    }

    String::new()
}

// --- Loading indicator ---

pub const LOADING_ID: &str = "reference_loading";

pub struct ReferenceLoading {
    symbol_col_offset: usize,
    start: std::time::Instant,
}

impl ReferenceLoading {
    const DOT_COUNT: usize = 5;
    const INTERVAL_MS: u128 = 150;
    const FILLED: &'static str = "●";
    const HOLLOW: &'static str = "○";

    pub fn new(editor: &Editor) -> Self {
        let symbol_col_offset = {
            let (view, doc) = helix_view::current_ref!(editor);
            let text = doc.text().slice(..);
            let cursor = doc.selection(view.id).primary().cursor(text);
            let line = text.char_to_line(cursor);
            let line_start = text.line_to_char(line);
            let cursor_col = cursor - line_start;
            let mut word_start = cursor_col;
            for i in (0..cursor_col).rev() {
                let ch = text.char(line_start + i);
                if categorize_char(ch) != CharCategory::Word {
                    break;
                }
                word_start = i;
            }
            cursor_col - word_start
        };

        ReferenceLoading {
            symbol_col_offset,
            start: std::time::Instant::now(),
        }
    }

    fn dots_label(&self) -> String {
        let elapsed = std::time::Instant::now().duration_since(self.start).as_millis();
        let active = (elapsed / Self::INTERVAL_MS) as usize % Self::DOT_COUNT;
        let mut s = String::with_capacity(Self::DOT_COUNT * 4 + 4);
        s.push(' ');
        for i in 0..Self::DOT_COUNT {
            if i == active {
                s.push_str(Self::FILLED);
            } else {
                s.push_str(Self::HOLLOW);
            }
        }
        s.push(' ');
        s
    }
}

impl Component for ReferenceLoading {
    fn handle_event(&mut self, event: &Event, _ctx: &mut Context) -> EventResult {
        if let Event::Key(_) = event {
            // Any key press cancels loading
            return EventResult::Consumed(Some(Box::new(
                |compositor: &mut compositor::Compositor, _cx: &mut compositor::Context| {
                    compositor.remove(LOADING_ID);
                },
            )));
        }
        EventResult::Ignored(None)
    }

    fn should_update(&self) -> bool {
        true
    }

    fn render(&mut self, viewport: Rect, surface: &mut Surface, cx: &mut Context) {
        let cursor_pos = cx.editor.cursor().0.unwrap_or_default();
        let cursor_row = cursor_pos.row as u16;
        let symbol_col = (cursor_pos.col as u16).saturating_sub(self.symbol_col_offset as u16);

        let y = if cursor_row + 2 < viewport.height {
            cursor_row + 1
        } else {
            cursor_row.saturating_sub(2)
        };

        let label = self.dots_label();
        let width = label.chars().count() as u16 + 2;

        let x = symbol_col.min(viewport.width.saturating_sub(width));
        let area = Rect::new(x, y, width, 1);

        let style = cx.editor.theme.get("ui.text");
        let bg = cx.editor.theme.get("ui.popup");
        surface.clear_with(area, bg);
        surface.set_stringn(area.x, area.y, &label, area.width as usize, style);
    }

    fn cursor(&self, _area: Rect, _editor: &Editor) -> (Option<Position>, CursorKind) {
        (None, CursorKind::Hidden)
    }

    fn id(&self) -> Option<&'static str> {
        Some(LOADING_ID)
    }
}

fn jump_to(
    editor: &mut Editor,
    path: &Path,
    range: lsp::Range,
    offset_encoding: OffsetEncoding,
    action: Action,
) {
    use helix_core::Selection;
    use helix_lsp::util::lsp_range_to_range;

    // Save jump position before navigating
    {
        let (view, doc) = helix_view::current!(editor);
        doc.append_changes_to_history(view);
        let jump = (doc.id(), doc.selection(view.id).clone());
        view.jumps.push(jump);
    }

    let doc = match editor.open(path, action) {
        Ok(id) => helix_view::doc_mut!(editor, &id),
        Err(err) => {
            editor.set_error(format!("failed to open: {:?}: {:?}", path, err));
            return;
        }
    };
    let view = helix_view::view_mut!(editor);
    if let Some(new_range) = lsp_range_to_range(doc.text(), range, offset_encoding) {
        doc.set_selection(view.id, Selection::single(new_range.head, new_range.anchor));
        if action.align_view(view, doc.id()) {
            editor.align_view_animated(Align::Center);
        }
    }
}
