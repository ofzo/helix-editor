use helix_core::Position;
use helix_view::graphics::{CursorKind, Rect};
use helix_view::Editor;
use tui::buffer::Buffer as Surface;

use crate::compositor::{Component, Context, Event, EventResult};

use super::terminal::TerminalPane;

/// A floating terminal overlay displayed centered on screen.
pub struct FloatTerminal {
    pub terminal: TerminalPane,
    id_str: &'static str,
}

impl FloatTerminal {
    pub fn new(terminal: TerminalPane) -> Self {
        Self {
            terminal,
            id_str: "float-terminal",
        }
    }

    pub fn new_with_id(terminal: TerminalPane, id_str: &'static str) -> Self {
        Self { terminal, id_str }
    }
}

impl Component for FloatTerminal {
    fn handle_event(&mut self, event: &Event, ctx: &mut Context) -> EventResult {
        // Auto-close when the shell process exits, and pre-warm a new terminal
        if self.terminal.has_exited() {
            let is_custom_cmd = self.id_str == "float-terminal-cmd";
            let id_str = self.id_str;
            return EventResult::Consumed(Some(Box::new(
                move |compositor: &mut crate::compositor::Compositor, cx: &mut Context| {
                    compositor.remove(id_str);
                    if let Some(editor_view) =
                        compositor.find::<crate::ui::EditorView>()
                    {
                        editor_view.start_terminal_prewarm();
                    }

                    if is_custom_cmd {
                        // Reload all open documents from disk
                        let scrolloff = cx.editor.config().scrolloff;
                        let view_id = view!(cx.editor).id;
                        let docs_view_ids: Vec<(
                            helix_view::DocumentId,
                            Vec<helix_view::ViewId>,
                        )> = cx
                            .editor
                            .documents_mut()
                            .map(|doc| {
                                let mut view_ids: Vec<_> =
                                    doc.selections().keys().cloned().collect();
                                if view_ids.is_empty() {
                                    doc.ensure_view_init(view_id);
                                    view_ids.push(view_id);
                                }
                                (doc.id(), view_ids)
                            })
                            .collect();

                        for (doc_id, view_ids) in docs_view_ids {
                            let doc = doc_mut!(cx.editor, &doc_id);
                            let view = view_mut!(cx.editor, view_ids[0]);
                            view.sync_changes(doc);
                            if let Err(error) =
                                doc.reload(view, &cx.editor.diff_providers)
                            {
                                cx.editor
                                    .set_error(format!("{}", error));
                                continue;
                            }
                            if let Some(path) = doc.path() {
                                cx.editor
                                    .language_servers
                                    .file_event_handler
                                    .file_changed(path.clone());
                            }
                            for view_id in view_ids {
                                let view = view_mut!(cx.editor, view_id);
                                if view.doc.eq(&doc_id) {
                                    let doc = doc_mut!(cx.editor, &doc_id);
                                    view.ensure_cursor_in_view(doc, scrolloff);
                                }
                            }
                        }
                        cx.editor
                            .set_status("Command exited. All buffers reloaded.");
                    }
                },
            )));
        }

        // Check for Escape key to close the float
        if let Event::Key(key) = event {
            use helix_view::keyboard::KeyCode;
            let id_str = self.id_str;
            if key.code == KeyCode::Esc
                && key.modifiers.is_empty()
                && !self.terminal.is_focus()
            {
                return EventResult::Consumed(Some(Box::new(
                    move |compositor: &mut crate::compositor::Compositor, _cx: &mut Context| {
                        compositor.remove(id_str);
                    },
                )));
            }
        }

        // A floating terminal is a full overlay — always ensure it has
        // focus so keys are forwarded to the PTY and never leak through
        // to the editor underneath.
        if !self.terminal.is_focus() {
            self.terminal.focus();
        }
        self.terminal.handle_event(event, ctx)
    }

    fn render(&mut self, area: Rect, surface: &mut Surface, cx: &mut Context) {
        // Don't render if shell has exited (will be removed on next event)
        if self.terminal.has_exited() {
            return;
        }

        // Draw border
        let border_style = cx.editor.theme.get("ui.popup");
        surface.clear_with(area, border_style);

        // Draw border frame
        let title = format!(" {} ", self.terminal.title());
        let top_left = area.x;
        let top_right = area.x + area.width.saturating_sub(1);
        let bottom_left = area.x;
        let bottom_right = area.x + area.width.saturating_sub(1);

        // Top border
        let border_chars_style = cx.editor.theme.try_get("ui.popup.border").unwrap_or(border_style);
        surface.set_string(top_left, area.y, "╭", border_chars_style);
        for x in (top_left + 1)..top_right {
            surface.set_string(x, area.y, "─", border_chars_style);
        }
        surface.set_string(top_right, area.y, "╮", border_chars_style);

        // Title
        let title_style = cx
            .editor
            .theme
            .try_get("ui.popup.header")
            .unwrap_or(border_chars_style);
        let title_x = top_left + 2;
        if title_x + title.len() as u16 <= top_right {
            surface.set_string(title_x, area.y, title, title_style);
        }

        // Side borders
        for y in (area.y + 1)..area.y + area.height.saturating_sub(1) {
            surface.set_string(top_left, y, "│", border_chars_style);
            surface.set_string(top_right, y, "│", border_chars_style);
        }

        // Bottom border
        surface.set_string(bottom_left, area.y + area.height.saturating_sub(1), "╰", border_chars_style);
        for x in (bottom_left + 1)..bottom_right {
            surface.set_string(x, area.y + area.height.saturating_sub(1), "─", border_chars_style);
        }
        surface.set_string(
            bottom_right,
            area.y + area.height.saturating_sub(1),
            "╯",
            border_chars_style,
        );

        // Inner area for terminal content
        let inner = Rect::new(
            area.x + 1,
            area.y + 1,
            area.width.saturating_sub(2),
            area.height.saturating_sub(2),
        );

        if inner.width > 0 && inner.height > 0 {
            self.terminal.render(inner, surface, cx);
        }
    }

    fn cursor(&self, area: Rect, editor: &Editor) -> (Option<Position>, CursorKind) {
        let inner = Rect::new(
            area.x + 1,
            area.y + 1,
            area.width.saturating_sub(2),
            area.height.saturating_sub(2),
        );
        self.terminal.cursor(inner, editor)
    }

    fn id(&self) -> Option<&'static str> {
        Some(self.id_str)
    }
}
