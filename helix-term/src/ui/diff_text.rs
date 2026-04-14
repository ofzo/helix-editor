use crate::compositor::{Callback, Component, Context, Event, EventResult};
use helix_view::graphics::Rect;
use helix_view::input::MouseEventKind;
use helix_view::{DocumentId, ViewId};
use tui::buffer::Buffer as Surface;
use tui::text::Spans;

/// The kind of a diff line, used to determine background color.
#[derive(Clone, Copy)]
pub enum DiffLineKind {
    Added,
    Removed,
}

/// A single line in a diff view, with a kind and styled spans.
pub struct DiffLine {
    pub kind: DiffLineKind,
    pub spans: Spans<'static>,
}

/// A full-width diff overlay anchored to a specific document line.
///
/// The overlay position is recalculated each frame based on where the anchor
/// line appears on screen, so it scrolls with the document content.
///
/// Uses `diff.minus.background` / `diff.plus.background` theme keys for
/// line background colors.
pub struct DiffOverlay {
    lines: Vec<DiffLine>,
    view_id: ViewId,
    doc_id: DocumentId,
    /// The char position in the document where the overlay is anchored.
    anchor_char_pos: usize,
    scroll_half_pages: usize,
}

impl DiffOverlay {
    pub fn new(
        lines: Vec<DiffLine>,
        view_id: ViewId,
        doc_id: DocumentId,
        anchor_char_pos: usize,
    ) -> Self {
        Self {
            lines,
            view_id,
            doc_id,
            anchor_char_pos,
            scroll_half_pages: 0,
        }
    }

    fn scroll_half_page_down(&mut self) {
        self.scroll_half_pages += 1;
    }

    fn scroll_half_page_up(&mut self) {
        self.scroll_half_pages = self.scroll_half_pages.saturating_sub(1);
    }
}

impl Component for DiffOverlay {
    fn id(&self) -> Option<&'static str> {
        Some("diff-overlay")
    }

    fn handle_event(&mut self, event: &Event, _cx: &mut Context) -> EventResult {
        let close_fn: Callback = Box::new(|compositor, _cx| {
            compositor.remove("diff-overlay");
        });

        match event {
            Event::Key(key) => {
                use crate::{ctrl, key};
                match *key {
                    key!(Esc) | ctrl!('c') => EventResult::Consumed(Some(close_fn)),
                    key!(PageDown) | ctrl!('d') => {
                        self.scroll_half_page_down();
                        EventResult::Consumed(None)
                    }
                    key!(PageUp) | ctrl!('u') => {
                        self.scroll_half_page_up();
                        EventResult::Consumed(None)
                    }
                    _ => EventResult::Ignored(Some(close_fn)),
                }
            }
            Event::Mouse(mouse) => match mouse.kind {
                MouseEventKind::ScrollDown => {
                    self.scroll_half_page_down();
                    EventResult::Consumed(None)
                }
                MouseEventKind::ScrollUp => {
                    self.scroll_half_page_up();
                    EventResult::Consumed(None)
                }
                MouseEventKind::Down(_) => EventResult::Ignored(Some(close_fn)),
                _ => EventResult::Ignored(None),
            },
            _ => EventResult::Ignored(None),
        }
    }

    fn render(&mut self, _viewport: Rect, surface: &mut Surface, cx: &mut Context) {
        let editor = &cx.editor;

        let Some(view) = editor.tree.try_get(self.view_id) else {
            return;
        };
        let Some(doc) = editor.documents.get(&self.doc_id) else {
            return;
        };

        // Find where the anchor line currently appears on screen
        let text = doc.text().slice(..);
        let Some(screen_pos) = view.screen_coords_at_pos(doc, text, self.anchor_char_pos) else {
            // Anchor line is off screen — don't render
            return;
        };

        let view_area = view.area;
        let inner = view.inner_area(doc);
        // screen_coords_at_pos returns coords relative to inner_area
        let y = inner.y + screen_pos.row as u16;
        let available_height = view_area.bottom().saturating_sub(y);
        let height = (self.lines.len() as u16).min(available_height);
        if height == 0 {
            return;
        }
        let area = Rect::new(view_area.x, y, view_area.width, height);

        let theme = &editor.theme;

        // Compute scroll
        let max_offset = self.lines.len().saturating_sub(area.height as usize);
        let half_page = (area.height / 2).max(1) as usize;
        let scroll = max_offset.min(self.scroll_half_pages * half_page);
        if half_page > 0 {
            self.scroll_half_pages = scroll / half_page;
        }

        // Clear the entire overlay area so editor content doesn't bleed through
        let bg_style = theme.get("ui.background");
        surface.clear_with(area, bg_style);

        let minus_bg = theme
            .try_get("diff.minus.background")
            .unwrap_or_else(|| theme.get("diff.minus"));
        let plus_bg = theme
            .try_get("diff.plus.background")
            .unwrap_or_else(|| theme.get("diff.plus"));

        for (i, line) in self.lines.iter().skip(scroll).enumerate() {
            if i as u16 >= area.height {
                break;
            }
            let y = area.y + i as u16;
            let line_rect = Rect::new(area.x, y, area.width, 1);

            let bg_style = match line.kind {
                DiffLineKind::Added => plus_bg,
                DiffLineKind::Removed => minus_bg,
            };
            surface.set_style(line_rect, bg_style);
            surface.set_spans(area.x, y, &line.spans, area.width);
        }
    }
}
