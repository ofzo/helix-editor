use crate::compositor::{Component, Context};
use helix_core::unicode::width::{UnicodeWidthChar, UnicodeWidthStr};
use helix_view::graphics::{Margin, Modifier, Rect};
use helix_view::info::Info;
use tui::buffer::Buffer as Surface;
use tui::widgets::{Block, Widget};

impl Component for Info {
    fn render(&mut self, viewport: Rect, surface: &mut Surface, cx: &mut Context) {
        let text_style = cx.editor.theme.get("ui.text.info");
        let popup_style = cx.editor.theme.get("ui.popup.info");

        // Calculate display dimensions
        let total_lines = self.text.lines().count();
        let display_lines = total_lines.min(16); // Max 16 lines for content
        let has_scrollbar = total_lines > 16;

        // Add space for footer (1 line empty + 1 line for hints)
        let footer_height = 2u16;
        let border_height = 2u16; // Top and bottom borders

        // Total height: content + empty line + footer + borders
        let height = display_lines as u16 + footer_height + border_height;
        // Dynamic width: min 30, max 60, including border and margin
        let width = (self.width + 2 + 2).clamp(30, 60);

        let area = viewport.intersection(Rect::new(
            viewport.width.saturating_sub(width),
            viewport.height.saturating_sub(height + 1), // +1 for combined statusline + command line
            width,
            height,
        ));
        surface.clear_with(area, popup_style);

        // Use block without title (title is shown in statusline instead)
        let block = Block::bordered()
            .border_style(popup_style);

        let margin = Margin::horizontal(1);
        let inner = block.inner(area).inner(margin);
        block.render(area, surface);

        // Get the lines to display based on scroll offset
        let lines: Vec<&str> = self.text.lines().collect();
        let end_offset = (self.scroll_offset + display_lines).min(total_lines);
        let visible_lines: Vec<&str> = lines[self.scroll_offset..end_offset].to_vec();

        // Render visible text with ellipsis truncation
        let content_area = inner.clip_bottom(footer_height);
        let max_line_width = content_area.width as usize;
        for (i, line) in visible_lines.iter().enumerate() {
            let y = content_area.y + i as u16;
            if y >= content_area.y + content_area.height {
                break;
            }
            let text_to_render = if line.width() > max_line_width {
                // Truncate with ellipsis
                let mut truncated = String::new();
                let mut current_width = 0;
                for ch in line.chars() {
                    let ch_width = ch.width().unwrap_or(1);
                    if current_width + ch_width + 3 > max_line_width {
                        // Add ellipsis
                        truncated.push_str("...");
                        break;
                    }
                    truncated.push(ch);
                    current_width += ch_width;
                }
                truncated
            } else {
                line.to_string()
            };
            surface.set_string(content_area.x, y, &text_to_render, text_style);
        }

        // Render footer with hints and range info (dim style)
        let footer_area = inner.clip_top(display_lines as u16 + 1); // +1 for empty line separator
        let dim_style = text_style.add_modifier(Modifier::DIM);
        let end_line = end_offset.min(total_lines);
        if has_scrollbar {
            // Left: scroll hint, Right: range info (e.g., "6-22/40")
            let left_text = "S-↑/↓ scroll";
            let right_text = format!("{}-{}/{}", self.scroll_offset + 1, end_line, total_lines);
            surface.set_string(footer_area.x, footer_area.y, left_text, dim_style);
            // Right-align the range info
            let right_x = footer_area.x + footer_area.width.saturating_sub(right_text.width() as u16);
            surface.set_string(right_x, footer_area.y, &right_text, dim_style);
        } else {
            let left_text = "esc close";
            let right_text = format!("{}/{}", total_lines, total_lines);
            surface.set_string(footer_area.x, footer_area.y, left_text, dim_style);
            let right_x = footer_area.x + footer_area.width.saturating_sub(right_text.width() as u16);
            surface.set_string(right_x, footer_area.y, &right_text, dim_style);
        }
    }
}
