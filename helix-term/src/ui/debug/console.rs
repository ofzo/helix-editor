use crate::compositor::{Component, Context};
use helix_view::graphics::Rect;
use helix_view::Editor;
use tui::buffer::Buffer as Surface;
use tui::text::{Span, Spans};

/// A single line of output in the debug console.
#[derive(Debug, Clone)]
pub struct ConsoleLine {
    pub category: OutputCategory,
    pub text: String,
}

/// Category of debug console output, matching DAP output categories.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputCategory {
    Console,
    Stdout,
    Stderr,
    Telemetry,
    /// User-entered expression and its result.
    UserInput,
    UserResult,
}

/// The debug console component, showing adapter output and allowing expression evaluation.
pub struct DebugConsole {
    /// Output lines buffer.
    lines: Vec<ConsoleLine>,
    /// Scroll offset from the bottom (0 = scrolled to bottom).
    scroll_offset: usize,
    /// Maximum number of lines to keep in the buffer.
    max_lines: usize,
    /// Current input text for expression evaluation.
    input: String,
    /// Cursor position within the input.
    input_cursor: usize,
    /// History of previously entered expressions.
    history: Vec<String>,
    /// Current position in history (-1 = current input).
    history_index: Option<usize>,
    /// Last rendered area for click detection.
    pub last_area: Rect,
}

impl Default for DebugConsole {
    fn default() -> Self {
        Self {
            lines: Vec::new(),
            scroll_offset: 0,
            max_lines: 10_000,
            input: String::new(),
            input_cursor: 0,
            history: Vec::new(),
            history_index: None,
            last_area: Rect::default(),
        }
    }
}

impl DebugConsole {
    pub fn new() -> Self {
        Self::default()
    }

    /// Push a line of output to the console.
    pub fn push_output(&mut self, category: OutputCategory, text: String) {
        // Split multi-line output into individual lines
        for line in text.lines() {
            self.lines.push(ConsoleLine {
                category,
                text: line.to_string(),
            });
        }
        // If a single newline with no content, still push an empty line
        if text.is_empty() {
            self.lines.push(ConsoleLine {
                category,
                text: String::new(),
            });
        }

        // Trim buffer if it exceeds max
        while self.lines.len() > self.max_lines {
            self.lines.remove(0);
        }

        // Auto-scroll to bottom when new output arrives (if already at bottom)
        if self.scroll_offset == 0 {
            // Already at bottom, stay there
        }
    }

    /// Clear all output.
    pub fn clear(&mut self) {
        self.lines.clear();
        self.scroll_offset = 0;
    }

    pub fn scroll_up(&mut self, amount: usize) {
        let max_scroll = self.lines.len().saturating_sub(1);
        self.scroll_offset = (self.scroll_offset + amount).min(max_scroll);
    }

    pub fn scroll_down(&mut self, amount: usize) {
        self.scroll_offset = self.scroll_offset.saturating_sub(amount);
    }

    pub fn scroll_to_bottom(&mut self) {
        self.scroll_offset = 0;
    }

    /// Get the current input text.
    pub fn input(&self) -> &str {
        &self.input
    }

    /// Take the current input, clearing it and adding to history.
    pub fn take_input(&mut self) -> String {
        let input = std::mem::take(&mut self.input);
        self.input_cursor = 0;
        self.history_index = None;
        if !input.is_empty() {
            self.history.push(input.clone());
        }
        input
    }

    /// Insert a character at the cursor position.
    pub fn insert_char(&mut self, ch: char) {
        self.input.insert(self.input_cursor, ch);
        self.input_cursor += ch.len_utf8();
    }

    /// Delete the character before the cursor.
    pub fn backspace(&mut self) {
        if self.input_cursor > 0 {
            let prev = self.input[..self.input_cursor]
                .char_indices()
                .next_back()
                .map(|(i, _)| i)
                .unwrap_or(0);
            self.input.drain(prev..self.input_cursor);
            self.input_cursor = prev;
        }
    }

    /// Move cursor left.
    pub fn move_cursor_left(&mut self) {
        if self.input_cursor > 0 {
            self.input_cursor = self.input[..self.input_cursor]
                .char_indices()
                .next_back()
                .map(|(i, _)| i)
                .unwrap_or(0);
        }
    }

    /// Move cursor right.
    pub fn move_cursor_right(&mut self) {
        if self.input_cursor < self.input.len() {
            self.input_cursor += self.input[self.input_cursor..]
                .chars()
                .next()
                .map(|c| c.len_utf8())
                .unwrap_or(0);
        }
    }

    /// Navigate history up.
    pub fn history_up(&mut self) {
        if self.history.is_empty() {
            return;
        }
        let idx = match self.history_index {
            None => self.history.len() - 1,
            Some(0) => return,
            Some(i) => i - 1,
        };
        self.history_index = Some(idx);
        self.input = self.history[idx].clone();
        self.input_cursor = self.input.len();
    }

    /// Navigate history down.
    pub fn history_down(&mut self) {
        match self.history_index {
            None => {}
            Some(i) if i + 1 >= self.history.len() => {
                self.history_index = None;
                self.input.clear();
                self.input_cursor = 0;
            }
            Some(i) => {
                self.history_index = Some(i + 1);
                self.input = self.history[i + 1].clone();
                self.input_cursor = self.input.len();
            }
        }
    }

    fn render_output(&self, surface: &mut Surface, area: Rect, ctx: &Context) {
        let text_style = ctx.editor.theme.get("ui.text");
        let stderr_style = ctx
            .editor
            .theme
            .try_get("ui.debug.console.stderr")
            .unwrap_or_else(|| ctx.editor.theme.get("error"));
        let input_style = ctx
            .editor
            .theme
            .try_get("ui.debug.console.input")
            .unwrap_or_else(|| ctx.editor.theme.get("ui.text.focus"));
        let result_style = ctx
            .editor
            .theme
            .try_get("ui.debug.console.result")
            .unwrap_or_else(|| ctx.editor.theme.get("ui.text.info"));

        if self.lines.is_empty() {
            let dim_style = ctx.editor.theme.get("ui.text.subdued");
            let span = Span::styled("Debug console ready", dim_style);
            surface.set_spans(area.x, area.y, &Spans::from(span), area.width);
            return;
        }

        let visible_lines = area.height as usize;
        let total = self.lines.len();
        let start = total
            .saturating_sub(visible_lines)
            .saturating_sub(self.scroll_offset);
        let end = (start + visible_lines).min(total);

        for (i, line) in self.lines[start..end].iter().enumerate() {
            let y = area.y + i as u16;
            if y >= area.y + area.height {
                break;
            }

            let (prefix, style) = match line.category {
                OutputCategory::Console => ("", text_style),
                OutputCategory::Stdout => ("", text_style),
                OutputCategory::Stderr => ("[err] ", stderr_style),
                OutputCategory::Telemetry => ("[tel] ", text_style),
                OutputCategory::UserInput => ("\u{f062} ", input_style),
                OutputCategory::UserResult => ("\u{f063} ", result_style),
            };

            let text = format!("{prefix}{}", line.text);
            let span = Span::styled(&text, style);
            surface.set_spans(area.x, y, &Spans::from(span), area.width);
        }
    }

    fn render_input_line(&self, surface: &mut Surface, area: Rect, ctx: &Context) {
        let is_focused = ctx.editor.debug_session.focus == helix_view::debug::DebugFocus::Console;

        if !is_focused {
            return;
        }

        let line_bg = ctx.editor.theme
            .try_get("ui.cursorline.insert")
            .unwrap_or_else(|| ctx.editor.theme.get("ui.cursorline.primary"));

        // Fill entire input line with background
        surface.clear_with(area, line_bg);

        if self.input.is_empty() {
            let placeholder_style = ctx.editor.theme.get("ui.text.subdued");
            let placeholder = "Type expression to evaluate...";
            let span = Span::styled(placeholder, placeholder_style);
            surface.set_spans(area.x + 1, area.y, &Spans::from(span), area.width.saturating_sub(1));
        } else {
            let input_style = ctx.editor.theme.get("ui.text");
            let input_span = Span::styled(&self.input, input_style);
            surface.set_spans(area.x + 1, area.y, &Spans::from(input_span), area.width.saturating_sub(1));
        }
    }
}

impl Component for DebugConsole {
    fn render(&mut self, area: Rect, surface: &mut Surface, ctx: &mut Context) {
        self.last_area = area;
        if area.height < 2 {
            return;
        }

        // Sync output from debug session into console lines
        let pending: Vec<_> = ctx.editor.debug_session.output_lines.drain(..).collect();
        for (category, text) in pending {
            let cat = match category.as_str() {
                "stderr" => OutputCategory::Stderr,
                "telemetry" => OutputCategory::Telemetry,
                "result" => OutputCategory::UserResult,
                _ => OutputCategory::Stdout,
            };
            self.push_output(cat, text);
        }

        let border_style = ctx.editor.theme.get("ui.window");
        let bg_style = ctx.editor.theme.get("ui.background");

        // Draw top border
        for x in area.x..area.x + area.width {
            surface.set_string(x, area.y, "─", border_style);
        }

        let content_area = Rect::new(area.x, area.y + 1, area.width, area.height.saturating_sub(1));
        surface.clear_with(content_area, bg_style);

        if content_area.height < 2 {
            return;
        }

        // Output area (all but last line)
        let output_area = Rect::new(
            content_area.x,
            content_area.y,
            content_area.width,
            content_area.height.saturating_sub(1),
        );
        self.render_output(surface, output_area, ctx);

        // Input line (last line)
        let input_y = content_area.y + content_area.height - 1;
        let input_area = Rect::new(content_area.x, input_y, content_area.width, 1);
        self.render_input_line(surface, input_area, ctx);
    }

    fn cursor(&self, area: Rect, ctx: &Editor) -> (Option<helix_core::Position>, helix_view::graphics::CursorKind) {
        if area.height < 3 {
            return (None, helix_view::graphics::CursorKind::Hidden);
        }

        // Only show cursor when console is focused
        if ctx.debug_session.focus != helix_view::debug::DebugFocus::Console {
            return (None, helix_view::graphics::CursorKind::Hidden);
        }

        let input_y = area.y + area.height - 1;
        let padding = 1u16;
        let cursor_x = area.x + padding + self.input_cursor as u16;

        (
            Some(helix_core::Position {
                row: input_y as usize,
                col: cursor_x as usize,
            }),
            helix_view::graphics::CursorKind::Block,
        )
    }
}
