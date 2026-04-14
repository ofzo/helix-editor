use helix_view::graphics::Rect;
use tui::{
    buffer::Buffer as Surface,
    widgets::{Block, Widget as _},
};

use crate::compositor::{Component, Context, Event, EventResult};

/// A centered modal confirmation dialog. Enter confirms, Esc cancels.
///
/// Layout:
/// ```text
/// +- Delete ----------------------------------------+
/// |                                                  |
/// | path/to/name.ext                                 |
/// +--------------------------------------------------+
/// ```
pub struct ConfirmDialog {
    title: String,
    message: String,
    on_confirm: Option<Box<dyn FnOnce(&mut Context)>>,
}

impl ConfirmDialog {
    pub fn new<F>(title: impl Into<String>, message: impl Into<String>, on_confirm: F) -> Self
    where
        F: FnOnce(&mut Context) + 'static,
    {
        Self {
            title: title.into(),
            message: message.into(),
            on_confirm: Some(Box::new(on_confirm)),
        }
    }
}

impl Component for ConfirmDialog {
    fn handle_event(&mut self, event: &Event, cx: &mut Context) -> EventResult {
        let key_event = match event {
            Event::Key(event) => *event,
            _ => return EventResult::Ignored(None),
        };

        use helix_view::keyboard::{KeyCode, KeyModifiers};
        match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Enter) => {
                if let Some(cb) = self.on_confirm.take() {
                    cb(cx);
                }
                let close: crate::compositor::Callback =
                    Box::new(|compositor, _cx| { compositor.pop(); });
                EventResult::Consumed(Some(close))
            }
            (KeyModifiers::NONE, KeyCode::Esc) => {
                let close: crate::compositor::Callback =
                    Box::new(|compositor, _cx| { compositor.pop(); });
                EventResult::Consumed(Some(close))
            }
            _ => EventResult::Consumed(None),
        }
    }

    fn render(&mut self, area: Rect, surface: &mut Surface, cx: &mut Context) {
        let min_width = ((area.width as u32) * 40 / 100) as u16;
        let max_width = ((area.width as u32) * 90 / 100) as u16;
        let content_width = min_width
            .max(self.title.len() as u16 + 4)
            .max(self.message.len() as u16 + 4)
            .min(max_width);

        // bordered box: border(1) + blank line + message line + border(1) = 4
        let width = content_width + 2; // +2 for left/right borders
        let height: u16 = 4;
        let popup = Rect {
            x: area.width.saturating_sub(width) / 2,
            y: area.height.saturating_sub(height) / 2,
            width: width.min(area.width),
            height: height.min(area.height),
        };

        let dialog_style = cx
            .editor
            .theme
            .try_get("ui.dialog")
            .unwrap_or_else(|| cx.editor.theme.get("ui.popup"));

        surface.clear_with(popup, dialog_style);

        let title_style = cx
            .editor
            .theme
            .try_get("ui.dialog.title")
            .unwrap_or(dialog_style);
        let block = Block::bordered()
            .title(tui::text::Span::styled(
                format!(" {} ", self.title),
                title_style,
            ))
            .border_style(dialog_style);
        block.render(popup, surface);
        let inner = Block::bordered().inner(popup);

        // Message line (after blank line, inside the box)
        surface.set_string(inner.x + 1, inner.y + 1, &self.message, dialog_style);
    }
}
