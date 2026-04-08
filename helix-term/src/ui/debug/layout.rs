use crate::compositor::{Component, Context};
use helix_view::graphics::Rect;
use helix_view::Editor;
use tui::buffer::Buffer as Surface;
use tui::text::{Span, Spans};

use super::console::DebugConsole;
use super::sidebar::DebugSidebar;

/// The overall debug layout manager that composes the sidebar and console
/// into the debug overlay. This component owns and coordinates the debug
/// UI panels.
pub struct DebugLayout {
    pub sidebar: DebugSidebar,
    pub console: DebugConsole,
}

impl Default for DebugLayout {
    fn default() -> Self {
        Self {
            sidebar: DebugSidebar::new(),
            console: DebugConsole::new(),
        }
    }
}

impl DebugLayout {
    pub fn new() -> Self {
        Self::default()
    }

    /// Calculate the sidebar area (right side of the editor).
    /// Returns (editor_area, sidebar_area) where editor_area is the remaining space.
    pub fn sidebar_area(total: Rect) -> (Rect, Rect) {
        // Sidebar takes ~30% of width, minimum 30 columns, maximum 60
        let sidebar_width = (total.width as u32 * 30 / 100)
            .max(30)
            .min(60)
            .min(total.width as u32) as u16;

        let editor_width = total.width.saturating_sub(sidebar_width);

        let editor_area = Rect::new(total.x, total.y, editor_width, total.height);
        let sidebar_area = Rect::new(
            total.x + editor_width,
            total.y,
            sidebar_width,
            total.height,
        );

        (editor_area, sidebar_area)
    }

    /// Calculate the console area (bottom of the editor).
    /// Returns (editor_area, console_area).
    pub fn console_area(total: Rect) -> (Rect, Rect) {
        // Console takes ~30% of height, minimum 5 rows, maximum 20
        let console_height = (total.height as u32 * 30 / 100)
            .max(5)
            .min(20)
            .min(total.height as u32) as u16;

        let editor_height = total.height.saturating_sub(console_height);

        let editor_area = Rect::new(total.x, total.y, total.width, editor_height);
        let console_area = Rect::new(
            total.x,
            total.y + editor_height,
            total.width,
            console_height,
        );

        (editor_area, console_area)
    }

    /// Calculate the full debug layout: editor, sidebar, and console areas.
    /// Layout:
    /// ```text
    /// ┌──────────────┬──────────┐
    /// │              │ Sidebar  │
    /// │   Editor     │          │
    /// │              │          │
    /// ├──────────────┴──────────┤
    /// │       Console           │
    /// └─────────────────────────┘
    /// ```
    pub fn compute_layout(total: Rect) -> DebugAreas {
        // First split top/bottom for console
        let (top_area, console_area) = Self::console_area(total);
        // Then split the top area for sidebar
        let (editor_area, sidebar_area) = Self::sidebar_area(top_area);

        DebugAreas {
            editor: editor_area,
            sidebar: sidebar_area,
            console: console_area,
        }
    }

    /// Render the debug status bar line showing session state.
    pub fn render_status(surface: &mut Surface, area: Rect, ctx: &Context) {
        let session = &ctx.editor.debug_session;
        let style = ctx
            .editor
            .theme
            .try_get("ui.debug.statusline")
            .unwrap_or_else(|| ctx.editor.theme.get("ui.statusline"));

        let conn_state = format!("{}", session.connection_state());
        let stop_info = session
            .stop_reason
            .as_deref()
            .map(|r| format!(" | Stopped: {r}"))
            .unwrap_or_default();

        let thread_info = session
            .active_thread_id
            .map(|id| format!(" | Thread {id}"))
            .unwrap_or_default();

        let frame_info = session
            .current_frame()
            .map(|f| format!(" | {}", f.name))
            .unwrap_or_default();

        let text = format!(" DEBUG [{conn_state}]{stop_info}{thread_info}{frame_info}");
        let span = Span::styled(&text, style);
        surface.set_spans(area.x, area.y, &Spans::from(span), area.width);
    }
}

/// The computed areas for the debug layout.
#[derive(Debug, Clone, Copy)]
pub struct DebugAreas {
    pub editor: Rect,
    pub sidebar: Rect,
    pub console: Rect,
}

impl Component for DebugLayout {
    fn render(&mut self, area: Rect, surface: &mut Surface, ctx: &mut Context) {
        let areas = Self::compute_layout(area);

        // Render sidebar
        self.sidebar.render(areas.sidebar, surface, ctx);

        // Render console
        self.console.render(areas.console, surface, ctx);
    }

    fn cursor(&self, area: Rect, ctx: &Editor) -> (Option<helix_core::Position>, helix_view::graphics::CursorKind) {
        let session = &ctx.debug_session;

        match session.focus {
            helix_view::debug::DebugFocus::Console => {
                let areas = Self::compute_layout(area);
                self.console.cursor(areas.console, ctx)
            }
            _ => (None, helix_view::graphics::CursorKind::Hidden),
        }
    }
}
