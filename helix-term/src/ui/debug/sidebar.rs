use crate::compositor::{Component, Context};
use helix_view::graphics::Rect;
use tui::buffer::Buffer as Surface;
use tui::text::{Span, Spans};

/// Sidebar section that is currently expanded.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SidebarSection {
    Variables,
    CallStack,
    Threads,
}

/// Action resulting from a mouse click on the sidebar.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SidebarAction {
    None,
    ToggleSection(SidebarSection),
    SelectFrame(usize),
    SelectThread(usize),
    ExpandVariable(usize, Vec<usize>),
    ShowVariableValue(String),
    ToolbarContinue,
    ToolbarStepOver,
    ToolbarStepIn,
    ToolbarStepOut,
    ToolbarStop,
}

/// The debug sidebar component, rendered on the right side of the debug layout.
/// Shows variables tree, call stack, and threads in collapsible sections.
pub struct DebugSidebar {
    /// Which sections are collapsed (by default all are expanded).
    collapsed: [bool; 3],
    /// Currently selected section.
    pub active_section: SidebarSection,
    /// Cursor position within the active section.
    pub cursor: usize,
    /// Scroll offset for the sidebar (used when content exceeds visible area).
    #[allow(dead_code)]
    scroll_offset: usize,
    /// Layout info from last render: (header_y, content_start_y, item_count) per section.
    section_layout: [(u16, u16, usize); 3],
    /// Content area from last render for bounds checking.
    last_content_area: Rect,
    /// Flat map of rendered variable items: (y_position, scope_idx, path_to_node).
    /// Built during render_variables for click and cursor mapping.
    pub var_flat_map: Vec<(u16, usize, Vec<usize>)>,
    /// Toolbar row y-position and button x-ranges: (x_start, x_end, action_index).
    /// action_index: 0=continue, 1=step_over, 2=step_in, 3=step_out, 4=stop
    toolbar_y: u16,
    toolbar_buttons: Vec<(u16, u16, usize)>,
}

impl Default for DebugSidebar {
    fn default() -> Self {
        Self {
            collapsed: [false, false, false],
            active_section: SidebarSection::Variables,
            cursor: 0,
            scroll_offset: 0,
            section_layout: [(0, 0, 0); 3],
            last_content_area: Rect::default(),
            var_flat_map: Vec::new(),
            toolbar_y: 0,
            toolbar_buttons: Vec::new(),
        }
    }
}

impl DebugSidebar {
    pub fn new() -> Self {
        Self::default()
    }

    fn section_index(section: SidebarSection) -> usize {
        match section {
            SidebarSection::Variables => 0,
            SidebarSection::CallStack => 1,
            SidebarSection::Threads => 2,
        }
    }

    fn section_from_index(index: usize) -> SidebarSection {
        match index {
            0 => SidebarSection::Variables,
            1 => SidebarSection::CallStack,
            _ => SidebarSection::Threads,
        }
    }

    pub fn toggle_section(&mut self, section: SidebarSection) {
        let idx = Self::section_index(section);
        self.collapsed[idx] = !self.collapsed[idx];
    }

    pub fn content_area(&self) -> Rect {
        self.last_content_area
    }

    pub fn is_collapsed(&self, section: SidebarSection) -> bool {
        self.collapsed[Self::section_index(section)]
    }

    pub fn move_cursor_up(&mut self) {
        self.cursor = self.cursor.saturating_sub(1);
    }

    pub fn move_cursor_down(&mut self, max: usize) {
        if self.cursor < max.saturating_sub(1) {
            self.cursor += 1;
        }
    }

    /// Handle a mouse click at the given screen position.
    /// Returns the action to take based on what was clicked.
    pub fn handle_mouse_click(&mut self, row: u16, col: u16) -> SidebarAction {
        let area = self.last_content_area;
        if area.width == 0 || area.height == 0 {
            return SidebarAction::None;
        }

        // Check bounds (include the border column)
        if col < area.x.saturating_sub(1) || col >= area.x + area.width
            || row < area.y || row >= area.y + area.height
        {
            return SidebarAction::None;
        }

        // Check toolbar row
        if row == self.toolbar_y {
            for &(x_start, x_end, action_idx) in &self.toolbar_buttons {
                if col >= x_start && col < x_end {
                    return match action_idx {
                        0 => SidebarAction::ToolbarContinue,
                        1 => SidebarAction::ToolbarStepOver,
                        2 => SidebarAction::ToolbarStepIn,
                        3 => SidebarAction::ToolbarStepOut,
                        4 => SidebarAction::ToolbarStop,
                        _ => SidebarAction::None,
                    };
                }
            }
            return SidebarAction::None;
        }

        // Check each section
        for i in 0..3 {
            let (header_y, content_start_y, item_count) = self.section_layout[i];
            if header_y == 0 && content_start_y == 0 && item_count == 0 {
                continue; // section not rendered
            }

            // Click on section header
            if row == header_y {
                return SidebarAction::ToggleSection(Self::section_from_index(i));
            }

            // Click on section content
            if item_count > 0 && row >= content_start_y && row < content_start_y + item_count as u16 {
                match i {
                    0 => {
                        // Variables section — match by absolute y position
                        if let Some((flat_idx, (_y, scope_idx, path))) = self.var_flat_map.iter().enumerate().find(|(_, (y, _, _))| *y == row) {
                            self.cursor = flat_idx;
                            self.active_section = SidebarSection::Variables;
                            return SidebarAction::ExpandVariable(*scope_idx, path.clone());
                        }
                    }
                    1 => {
                        let item_index = (row - content_start_y) as usize;
                        return SidebarAction::SelectFrame(item_index);
                    }
                    2 => {
                        let item_index = (row - content_start_y) as usize;
                        return SidebarAction::SelectThread(item_index);
                    }
                    _ => {}
                }
            }
        }

        SidebarAction::None
    }

    fn render_toolbar(
        &mut self,
        surface: &mut Surface,
        x: u16,
        y: u16,
        width: u16,
        is_stopped: bool,
        toolbar_style: helix_view::graphics::Style,
        dim_style: helix_view::graphics::Style,
    ) {
        self.toolbar_y = y;
        self.toolbar_buttons.clear();

        // Fill background
        for col in x..x + width {
            surface.set_string(col, y, " ", toolbar_style);
        }

        // Button definitions: (label_str, action_index)
        let buttons: &[(&str, usize)] = &[
            ("\u{f04b}(c) ", 0),    // continue
            ("\u{f051}(n) ", 1),    // step over (next)
            ("\u{f04e}(s) ", 2),    // step in
            ("\u{f04a}(o) ", 3),    // step out
            ("\u{f04d}(q) ", 4),    // stop
        ];

        let mut cx = x + 1;
        for &(label, action_idx) in buttons {
            let btn_width = label.chars().count() as u16;
            if cx + btn_width > x + width {
                break;
            }
            let style = if is_stopped || action_idx == 4 {
                toolbar_style
            } else {
                dim_style
            };
            let btn_start = cx;
            let span = Span::styled(label, style);
            surface.set_spans(cx, y, &Spans::from(span), btn_width);
            cx += btn_width;
            self.toolbar_buttons.push((btn_start, cx, action_idx));
        }
    }

    fn render_section_header(
        surface: &mut Surface,
        x: u16,
        y: u16,
        width: u16,
        title: &str,
        collapsed: bool,
        header_style: helix_view::graphics::Style,
    ) {
        // Fill the entire row with header style to create a visual separator bar
        for col in x..x + width {
            surface.set_string(col, y, " ", header_style);
        }
        let indicator = if collapsed { "▶ " } else { "▼ " };
        let text = format!("{indicator}{title}");
        let span = Span::styled(&text, header_style);
        surface.set_spans(x, y, &Spans::from(span), width);
    }

    fn render_variables(
        &mut self,
        surface: &mut Surface,
        area: Rect,
        ctx: &Context,
    ) -> u16 {
        let session = &ctx.editor.debug_session;
        let text_style = ctx.editor.theme.get("ui.text");
        let dim_style = ctx.editor.theme.get("ui.text.subdued");
        let selected_style = ctx.editor.theme.get("ui.menu.selected");
        let scope_style = ctx
            .editor
            .theme
            .try_get("ui.debug.scope")
            .unwrap_or_else(|| ctx.editor.theme.get("ui.text.focus"));

        if session.variables.is_empty() {
            let msg = if session.is_stopped {
                "  No variables"
            } else {
                "  (not stopped)"
            };
            let span = Span::styled(msg, dim_style);
            surface.set_spans(area.x, area.y, &Spans::from(span), area.width);
            return 1;
        }

        // Build flat list of visible items: scope headers + variable nodes
        self.var_flat_map.clear();
        let mut lines_used: u16 = 0;

        for (scope_idx, (scope_name, nodes)) in session.variables.iter().enumerate() {
            if lines_used >= area.height {
                break;
            }

            // Render scope name as a sub-header
            let scope_text = format!("  {scope_name}:");
            let span = Span::styled(&scope_text, scope_style);
            surface.set_spans(area.x, area.y + lines_used, &Spans::from(span), area.width);
            lines_used += 1;

            Self::render_variable_nodes(
                nodes,
                surface,
                area,
                &mut lines_used,
                scope_idx,
                &mut Vec::new(),
                &mut self.var_flat_map,
                text_style,
                selected_style,
                self.active_section == SidebarSection::Variables,
                self.cursor,
            );
        }

        lines_used.max(1)
    }

    fn render_variable_nodes(
        nodes: &[helix_view::debug::session::VariableNode],
        surface: &mut Surface,
        area: Rect,
        lines_used: &mut u16,
        scope_idx: usize,
        path: &mut Vec<usize>,
        flat_map: &mut Vec<(u16, usize, Vec<usize>)>,
        text_style: helix_view::graphics::Style,
        selected_style: helix_view::graphics::Style,
        is_active_section: bool,
        cursor: usize,
    ) {
        for (i, node) in nodes.iter().enumerate() {
            if *lines_used >= area.height {
                break;
            }

            path.push(i);
            let flat_idx = flat_map.len();
            let abs_y = area.y + *lines_used;
            flat_map.push((abs_y, scope_idx, path.clone()));

            let indent = "  ".repeat(node.depth + 2);
            let indicator = if node.is_expandable() {
                if node.expanded { "▼ " } else { "▶ " }
            } else {
                "  "
            };

            let var = &node.variable;
            let type_hint = var
                .ty
                .as_deref()
                .map(|t| format!(" ({t})"))
                .unwrap_or_default();

            let max_val_chars = area.width.saturating_sub(8) as usize;
            let val = if var.value.chars().count() > max_val_chars {
                let truncated: String = var.value.chars().take(max_val_chars.saturating_sub(3)).collect();
                format!("{truncated}...")
            } else {
                var.value.clone()
            };

            let text = format!("{indent}{indicator}{} = {val}{type_hint}", var.name);
            let style = if is_active_section && flat_idx == cursor {
                selected_style
            } else {
                text_style
            };
            let span = Span::styled(&text, style);
            surface.set_spans(area.x, area.y + *lines_used, &Spans::from(span), area.width);
            *lines_used += 1;

            if node.expanded && !node.children.is_empty() {
                Self::render_variable_nodes(
                    &node.children,
                    surface,
                    area,
                    lines_used,
                    scope_idx,
                    path,
                    flat_map,
                    text_style,
                    selected_style,
                    is_active_section,
                    cursor,
                );
            }

            path.pop();
        }
    }

    fn render_call_stack(
        &self,
        surface: &mut Surface,
        area: Rect,
        ctx: &Context,
    ) -> u16 {
        let session = &ctx.editor.debug_session;
        let text_style = ctx.editor.theme.get("ui.text");
        let selected_style = ctx.editor.theme.get("ui.menu.selected");
        let dim_style = ctx.editor.theme.get("ui.text.subdued");
        let mut lines_used: u16 = 0;

        if session.stack_frames.is_empty() {
            let span = Span::styled("  No stack frames", dim_style);
            surface.set_spans(area.x, area.y, &Spans::from(span), area.width);
            return 1;
        }

        for (i, frame) in session.stack_frames.iter().enumerate() {
            if lines_used >= area.height {
                break;
            }

            let is_deemphasized = matches!(
                frame.presentation_hint,
                Some(helix_dap::helix_dap_types::StackFramePresentationHint::Deemphasize)
                | Some(helix_dap::helix_dap_types::StackFramePresentationHint::Subtle)
            );

            let style = if i == session.active_frame_index {
                selected_style
            } else if is_deemphasized {
                dim_style
            } else {
                text_style
            };

            let indicator = if i == session.active_frame_index {
                "→ "
            } else {
                "  "
            };

            let source_info = frame
                .source
                .as_ref()
                .and_then(|s| s.name.as_deref())
                .unwrap_or("??");

            let text = format!("{indicator}{}  {source_info}:{}", frame.name, frame.line);
            let span = Span::styled(&text, style);
            surface.set_spans(area.x, area.y + lines_used, &Spans::from(span), area.width);
            lines_used += 1;
        }

        lines_used.max(1)
    }

    fn render_threads(
        &self,
        surface: &mut Surface,
        area: Rect,
        ctx: &Context,
    ) -> u16 {
        let session = &ctx.editor.debug_session;
        let text_style = ctx.editor.theme.get("ui.text");
        let selected_style = ctx.editor.theme.get("ui.menu.selected");
        let dim_style = ctx.editor.theme.get("ui.text.subdued");
        let mut lines_used: u16 = 0;

        if session.threads.is_empty() {
            let span = Span::styled("  No threads", dim_style);
            surface.set_spans(area.x, area.y, &Spans::from(span), area.width);
            return 1;
        }

        for (_i, thread) in session.threads.iter().enumerate() {
            if lines_used >= area.height {
                break;
            }

            let is_active = session.active_thread_id == Some(thread.id);

            let style = if is_active {
                selected_style
            } else {
                text_style
            };

            let indicator = if is_active { "● " } else { "○ " };
            let text = format!("{indicator}{} (id: {})", thread.name, thread.id);
            let span = Span::styled(&text, style);
            surface.set_spans(area.x, area.y + lines_used, &Spans::from(span), area.width);
            lines_used += 1;
        }

        lines_used.max(1)
    }
}

impl Component for DebugSidebar {
    fn render(&mut self, area: Rect, surface: &mut Surface, ctx: &mut Context) {
        let border_style = ctx.editor.theme.get("ui.window");
        let header_style = ctx.editor.theme.get("ui.text.focus");
        let bg_style = ctx.editor.theme.get("ui.background");

        // Draw left border
        for y in area.y..area.y + area.height {
            surface.set_string(area.x, y, "│", border_style);
        }

        let content_area = Rect::new(area.x + 1, area.y, area.width.saturating_sub(1), area.height);
        surface.clear_with(content_area, bg_style);

        // Store content area for mouse hit-testing
        self.last_content_area = content_area;
        // Reset section layout
        self.section_layout = [(0, 0, 0); 3];

        let mut y = content_area.y;

        // Render toolbar at the top
        let is_stopped = ctx.editor.debug_session.is_stopped;
        let toolbar_style = ctx.editor.theme.get("ui.statusline.inactive");
        let dim_style = ctx.editor.theme.get("ui.text.subdued");
        self.render_toolbar(surface, content_area.x, y, content_area.width, is_stopped, toolbar_style, dim_style);
        y += 1;

        if y >= content_area.y + content_area.height {
            return;
        }

        // Count items for each section (for layout tracking)
        let session = &ctx.editor.debug_session;
        let var_item_count: usize = session.variables.iter()
            .map(|(_, vars)| 1 + vars.len()) // scope header + vars
            .sum::<usize>()
            .max(1); // at least 1 for empty message
        let stack_item_count = session.stack_frames.len().max(1);
        let thread_item_count = session.threads.len().max(1);

        // Variables section
        let vars_header_y = y;
        Self::render_section_header(
            surface,
            content_area.x,
            y,
            content_area.width,
            "Variables",
            self.is_collapsed(SidebarSection::Variables),
            header_style,
        );
        y += 1;

        let vars_content_y = y;
        let mut vars_rendered = 0;
        if !self.is_collapsed(SidebarSection::Variables) && y < content_area.y + content_area.height {
            let remaining = content_area.height.saturating_sub(y - content_area.y);
            let var_area = Rect::new(content_area.x, y, content_area.width, remaining.min(8));
            let used = self.render_variables(surface, var_area, ctx);
            vars_rendered = used as usize;
            y += used;
        }
        self.section_layout[0] = (vars_header_y, vars_content_y, vars_rendered.min(var_item_count));

        if y >= content_area.y + content_area.height {
            return;
        }

        // Separator
        y += 1;
        if y >= content_area.y + content_area.height {
            return;
        }

        // Call Stack section
        let stack_header_y = y;
        Self::render_section_header(
            surface,
            content_area.x,
            y,
            content_area.width,
            "Call Stack",
            self.is_collapsed(SidebarSection::CallStack),
            header_style,
        );
        y += 1;

        let stack_content_y = y;
        let mut stack_rendered = 0;
        if !self.is_collapsed(SidebarSection::CallStack) && y < content_area.y + content_area.height {
            let remaining = content_area.height.saturating_sub(y - content_area.y);
            let stack_area = Rect::new(content_area.x, y, content_area.width, remaining.min(10));
            let used = self.render_call_stack(surface, stack_area, ctx);
            stack_rendered = used as usize;
            y += used;
        }
        self.section_layout[1] = (stack_header_y, stack_content_y, stack_rendered.min(stack_item_count));

        if y >= content_area.y + content_area.height {
            return;
        }

        // Separator
        y += 1;
        if y >= content_area.y + content_area.height {
            return;
        }

        // Threads section
        let threads_header_y = y;
        Self::render_section_header(
            surface,
            content_area.x,
            y,
            content_area.width,
            "Threads",
            self.is_collapsed(SidebarSection::Threads),
            header_style,
        );
        y += 1;

        let threads_content_y = y;
        let mut threads_rendered = 0;
        if !self.is_collapsed(SidebarSection::Threads) && y < content_area.y + content_area.height {
            let remaining = content_area.height.saturating_sub(y - content_area.y);
            let threads_area = Rect::new(content_area.x, y, content_area.width, remaining);
            let used = self.render_threads(surface, threads_area, ctx);
            threads_rendered = used as usize;
        }
        self.section_layout[2] = (threads_header_y, threads_content_y, threads_rendered.min(thread_item_count));
    }
}
