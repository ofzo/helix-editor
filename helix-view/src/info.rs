use crate::register::Registers;
use helix_core::unicode::width::UnicodeWidthStr;
use std::{borrow::Cow, fmt::Write};

#[derive(Debug)]
/// Info box used in editor. Rendering logic will be in other crate.
pub struct Info {
    /// Title shown at top.
    pub title: Cow<'static, str>,
    /// Text body, should contain newlines.
    pub text: String,
    /// Body width.
    pub width: u16,
    /// Body height (total lines in text).
    pub height: u16,
    /// Current scroll offset (number of lines scrolled).
    pub scroll_offset: usize,
}

/// Maximum number of lines to display in the info panel.
const MAX_DISPLAY_LINES: usize = 16;

impl Info {
    pub fn new<T, K, V>(title: T, body: &[(K, V)]) -> Self
    where
        T: Into<Cow<'static, str>>,
        K: AsRef<str>,
        V: AsRef<str>,
    {
        let title = title.into();
        if body.is_empty() {
            return Self {
                height: 1,
                width: title.len() as u16,
                text: "".to_string(),
                title,
                scroll_offset: 0,
            };
        }

        let item_width = body
            .iter()
            .map(|(item, _)| item.as_ref().width())
            .max()
            .unwrap();
        let mut text = String::new();

        for (item, desc) in body {
            // Single space between key and description (aligned by max item width)
            let _ = writeln!(
                text,
                "{:width$} {}",
                item.as_ref(),
                desc.as_ref(),
                width = item_width
            );
        }

        Self {
            title,
            width: text.lines().map(|l| l.width()).max().unwrap() as u16,
            height: text.lines().count() as u16,
            text,
            scroll_offset: 0,
        }
    }

    /// Scroll up by one line.
    pub fn scroll_up(&mut self) {
        self.scroll_offset = self.scroll_offset.saturating_sub(1);
    }

    /// Scroll down by one line.
    pub fn scroll_down(&mut self) {
        let total_lines = self.text.lines().count();
        let max_offset = total_lines.saturating_sub(MAX_DISPLAY_LINES);
        self.scroll_offset = (self.scroll_offset + 1).min(max_offset);
    }

    /// Get the number of display lines (min of total lines and max).
    pub fn display_lines(&self) -> usize {
        self.text.lines().count().min(MAX_DISPLAY_LINES)
    }

    /// Check if scrolling is possible.
    pub fn can_scroll_up(&self) -> bool {
        self.scroll_offset > 0
    }

    /// Check if scrolling down is possible.
    pub fn can_scroll_down(&self) -> bool {
        let total_lines = self.text.lines().count();
        self.scroll_offset + MAX_DISPLAY_LINES < total_lines
    }

    pub fn from_registers(title: impl Into<Cow<'static, str>>, registers: &Registers) -> Self {
        let body: Vec<_> = registers
            .iter_preview()
            .map(|(ch, preview)| (ch.to_string(), preview))
            .collect();

        let mut infobox = Self::new(title, &body);
        infobox.width = 30; // copied content could be very long
        infobox
    }
}
