use helix_core::Position;
use helix_view::{
    graphics::{CursorKind, Rect},
    Editor,
};
use tui::{
    buffer::Buffer as Surface,
    widgets::{Block, Widget as _},
};

use crate::compositor::{Component, Context, Event, EventResult};

use super::Prompt;

/// A centered modal dialog with a bordered box, title in the border,
/// and a text input field inside.
///
/// Layout:
/// ```text
/// +- Title ----------------------------------------+
/// |                                                 |
/// | input text here█                                |
/// +-------------------------------------------------+
/// ```
pub struct InputDialog {
    title: String,
    prompt: Prompt,
    /// Cached area where the prompt was last rendered, used for cursor().
    prompt_area: Rect,
}

impl InputDialog {
    pub fn new(title: impl Into<String>, prompt: Prompt) -> Self {
        Self {
            title: title.into(),
            // Disable auto-close so we can keep the dialog open on validation errors.
            prompt: prompt.auto_close(false),
            prompt_area: Rect::default(),
        }
    }
}

impl Component for InputDialog {
    fn handle_event(&mut self, event: &Event, cx: &mut Context) -> EventResult {
        // Forward the event to the embedded Prompt.
        let result = self.prompt.handle_event(event, cx);

        // When the prompt handles Enter (Validate), it calls the callback but
        // does NOT close (auto_close is false). We decide: close on success,
        // stay open on error so the user can fix their input.
        if let Event::Key(key_event) = event {
            use helix_view::keyboard::KeyCode;
            if key_event.code == KeyCode::Enter {
                if cx.editor.is_err() {
                    return EventResult::Consumed(None);
                }
                let close: crate::compositor::Callback =
                    Box::new(|compositor, _cx| { compositor.pop(); });
                return EventResult::Consumed(Some(close));
            }
        }

        result
    }

    fn render(&mut self, area: Rect, surface: &mut Surface, cx: &mut Context) {
        let min_width = ((area.width as u32) * 40 / 100) as u16;
        let max_width = ((area.width as u32) * 90 / 100) as u16;
        let content_width = min_width.max(self.title.len() as u16 + 4).min(max_width);

        // bordered box: border(1) + blank line + input line + border(1) = 4
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

        // Prompt input line (after blank line, inside the box)
        let prompt_area = Rect {
            x: inner.x + 1,
            y: inner.y + 1,
            width: inner.width.saturating_sub(2),
            height: 1,
        };
        self.prompt_area = prompt_area;
        self.prompt.render_prompt(prompt_area, surface, cx);
    }

    fn cursor(&self, _area: Rect, editor: &Editor) -> (Option<Position>, CursorKind) {
        self.prompt.cursor(self.prompt_area, editor)
    }
}
