use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use alacritty_terminal::event::{Event as TermEvent, EventListener, WindowSize};
use alacritty_terminal::event_loop::{EventLoop, Msg};
use alacritty_terminal::grid::Dimensions;
use alacritty_terminal::index::{Column, Line};
use alacritty_terminal::sync::FairMutex;
use alacritty_terminal::term::cell::{Cell, Flags as CellFlags};
use alacritty_terminal::term::{Config as TermConfig, Term};
use alacritty_terminal::tty;
use alacritty_terminal::vte::ansi::{Color as AnsiColor, CursorShape, NamedColor};

use helix_core::Position;
use helix_view::graphics::{Color, CursorKind, Modifier, Rect, Style};
use helix_view::input::KeyEvent;
use helix_view::keyboard::{KeyCode, KeyModifiers};
use helix_view::Editor;
use tui::buffer::Buffer as Surface;

use crate::compositor::{Component, Context, Event, EventResult};

/// Event proxy that requests redraws when the terminal updates.
/// Tracks child process exit via a shared atomic flag.
#[derive(Clone)]
struct EventProxy {
    exited: Arc<AtomicBool>,
}

impl EventListener for EventProxy {
    fn send_event(&self, event: TermEvent) {
        if matches!(event, TermEvent::Exit | TermEvent::ChildExit(_)) {
            self.exited.store(true, Ordering::Relaxed);
        }
        helix_event::request_redraw();
    }
}

/// Simple Dimensions implementation for Term creation and resize.
struct TermSize {
    columns: usize,
    screen_lines: usize,
}

impl TermSize {
    fn new(columns: usize, screen_lines: usize) -> Self {
        Self {
            columns,
            screen_lines,
        }
    }
}

impl Dimensions for TermSize {
    fn total_lines(&self) -> usize {
        self.screen_lines
    }

    fn screen_lines(&self) -> usize {
        self.screen_lines
    }

    fn columns(&self) -> usize {
        self.columns
    }

    fn last_column(&self) -> Column {
        Column(self.columns.saturating_sub(1))
    }

    fn topmost_line(&self) -> Line {
        Line(0)
    }

    fn bottommost_line(&self) -> Line {
        Line(self.screen_lines as i32 - 1)
    }

    fn history_size(&self) -> usize {
        0
    }
}

/// An interactive terminal pane powered by alacritty_terminal.
///
/// Used by both the bottom panel terminal and the floating terminal overlay.
pub struct TerminalPane {
    term: Arc<FairMutex<Term<EventProxy>>>,
    sender: alacritty_terminal::event_loop::EventLoopSender,
    exited: Arc<AtomicBool>,
    title: String,
    focus: bool,
    open: bool,
    panel_height: u16,
    last_cols: u16,
    last_rows: u16,
}

impl TerminalPane {
    pub fn new(rows: u16, cols: u16, panel_height: u16) -> anyhow::Result<Self> {
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string());
        let title = std::env::current_dir()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|_| "Terminal".to_string());
        Self::new_with_cmd(rows, cols, panel_height, shell, vec![], title)
    }

    /// Create a terminal pane running a custom command.
    pub fn new_with_cmd(
        rows: u16,
        cols: u16,
        panel_height: u16,
        cmd: String,
        args: Vec<String>,
        title: String,
    ) -> anyhow::Result<Self> {
        let exited = Arc::new(AtomicBool::new(false));
        let event_proxy = EventProxy {
            exited: Arc::clone(&exited),
        };

        let config = TermConfig::default();
        let size = TermSize::new(cols.max(2) as usize, rows.max(2) as usize);
        let term = Term::new(config, &size, event_proxy.clone());
        let term = Arc::new(FairMutex::new(term));

        let pty_config = tty::Options {
            shell: Some(tty::Shell::new(cmd, args)),
            working_directory: Some(helix_stdx::env::current_working_dir()),
            drain_on_exit: false,
            env: HashMap::new(),
        };

        let window_size = WindowSize {
            num_lines: rows,
            num_cols: cols,
            cell_width: 1,
            cell_height: 1,
        };

        let pty = tty::new(&pty_config, window_size, 0)?;
        let event_loop = EventLoop::new(Arc::clone(&term), event_proxy, pty, false, false)?;
        let sender = event_loop.channel();
        event_loop.spawn();

        Ok(Self {
            term,
            sender,
            exited,
            title,
            focus: true,
            open: true,
            panel_height,
            last_cols: cols,
            last_rows: rows,
        })
    }

    fn write_to_pty(&self, data: &[u8]) {
        let _ = self.sender.send(Msg::Input(data.to_vec().into()));
    }

    pub fn focus(&mut self) {
        self.focus = true;
        self.open = true;
    }

    pub fn unfocus(&mut self) {
        self.focus = false;
    }

    pub fn is_focus(&self) -> bool {
        self.focus
    }

    pub fn is_opened(&self) -> bool {
        self.open
    }

    pub fn close(&mut self) {
        self.focus = false;
        self.open = false;
    }

    pub fn toggle(&mut self) {
        if self.open {
            if self.focus {
                self.unfocus();
            } else {
                self.focus();
            }
        } else {
            self.focus();
        }
    }

    pub fn panel_height(&self) -> u16 {
        self.panel_height
    }

    pub fn title(&self) -> &str {
        &self.title
    }

    /// Returns true if the shell process has exited.
    pub fn has_exited(&self) -> bool {
        self.exited.load(Ordering::Relaxed)
    }

    fn resize(&mut self, rows: u16, cols: u16) {
        if rows == self.last_rows && cols == self.last_cols {
            return;
        }
        if rows < 2 || cols < 2 {
            return;
        }
        self.last_rows = rows;
        self.last_cols = cols;

        let size = TermSize::new(cols as usize, rows as usize);
        self.term.lock().resize(size);

        let window_size = WindowSize {
            num_lines: rows,
            num_cols: cols,
            cell_width: 1,
            cell_height: 1,
        };
        let _ = self.sender.send(Msg::Resize(window_size));
    }

    fn key_to_bytes(key: &KeyEvent) -> Option<Vec<u8>> {
        let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
        let alt = key.modifiers.contains(KeyModifiers::ALT);

        let bytes = match key.code {
            KeyCode::Char(c) => {
                if ctrl {
                    // Ctrl+A..Z maps to 0x01..0x1A
                    let byte = (c.to_ascii_lowercase() as u8).wrapping_sub(b'a').wrapping_add(1);
                    if alt {
                        vec![0x1b, byte]
                    } else {
                        vec![byte]
                    }
                } else if alt {
                    let mut buf = vec![0x1b];
                    let mut char_buf = [0u8; 4];
                    buf.extend_from_slice(c.encode_utf8(&mut char_buf).as_bytes());
                    buf
                } else {
                    let mut buf = [0u8; 4];
                    c.encode_utf8(&mut buf).as_bytes().to_vec()
                }
            }
            KeyCode::Enter => vec![0x0d],
            KeyCode::Backspace => vec![0x7f],
            KeyCode::Tab => {
                if key.modifiers.contains(KeyModifiers::SHIFT) {
                    b"\x1b[Z".to_vec()
                } else {
                    vec![0x09]
                }
            }
            KeyCode::Esc => vec![0x1b],
            KeyCode::Up => b"\x1b[A".to_vec(),
            KeyCode::Down => b"\x1b[B".to_vec(),
            KeyCode::Right => b"\x1b[C".to_vec(),
            KeyCode::Left => b"\x1b[D".to_vec(),
            KeyCode::Home => b"\x1b[H".to_vec(),
            KeyCode::End => b"\x1b[F".to_vec(),
            KeyCode::PageUp => b"\x1b[5~".to_vec(),
            KeyCode::PageDown => b"\x1b[6~".to_vec(),
            KeyCode::Delete => b"\x1b[3~".to_vec(),
            KeyCode::Insert => b"\x1b[2~".to_vec(),
            KeyCode::F(n) => match n {
                1 => b"\x1bOP".to_vec(),
                2 => b"\x1bOQ".to_vec(),
                3 => b"\x1bOR".to_vec(),
                4 => b"\x1bOS".to_vec(),
                5 => b"\x1b[15~".to_vec(),
                6 => b"\x1b[17~".to_vec(),
                7 => b"\x1b[18~".to_vec(),
                8 => b"\x1b[19~".to_vec(),
                9 => b"\x1b[20~".to_vec(),
                10 => b"\x1b[21~".to_vec(),
                11 => b"\x1b[23~".to_vec(),
                12 => b"\x1b[24~".to_vec(),
                _ => return None,
            },
            _ => return None,
        };

        Some(bytes)
    }

    fn convert_color(
        color: &AnsiColor,
        colors: &alacritty_terminal::term::color::Colors,
    ) -> Color {
        match color {
            AnsiColor::Spec(rgb) => Color::Rgb(rgb.r, rgb.g, rgb.b),
            AnsiColor::Indexed(idx) => {
                if let Some(rgb) = colors[*idx as usize] {
                    Color::Rgb(rgb.r, rgb.g, rgb.b)
                } else {
                    Color::Indexed(*idx)
                }
            }
            AnsiColor::Named(name) => {
                if let Some(rgb) = colors[*name] {
                    Color::Rgb(rgb.r, rgb.g, rgb.b)
                } else {
                    Self::named_color_to_helix(*name)
                }
            }
        }
    }

    fn named_color_to_helix(color: NamedColor) -> Color {
        match color {
            NamedColor::Black | NamedColor::DimBlack => Color::Black,
            NamedColor::Red | NamedColor::DimRed => Color::Red,
            NamedColor::Green | NamedColor::DimGreen => Color::Green,
            NamedColor::Yellow | NamedColor::DimYellow => Color::Yellow,
            NamedColor::Blue | NamedColor::DimBlue => Color::Blue,
            NamedColor::Magenta | NamedColor::DimMagenta => Color::Magenta,
            NamedColor::Cyan | NamedColor::DimCyan => Color::Cyan,
            NamedColor::White | NamedColor::DimWhite => Color::Gray,
            NamedColor::BrightBlack => Color::Gray,
            NamedColor::BrightRed => Color::LightRed,
            NamedColor::BrightGreen => Color::LightGreen,
            NamedColor::BrightYellow => Color::LightYellow,
            NamedColor::BrightBlue => Color::LightBlue,
            NamedColor::BrightMagenta => Color::LightMagenta,
            NamedColor::BrightCyan => Color::LightCyan,
            NamedColor::BrightWhite => Color::White,
            NamedColor::Foreground
            | NamedColor::BrightForeground
            | NamedColor::DimForeground => Color::Reset,
            NamedColor::Background | NamedColor::Cursor => Color::Reset,
        }
    }

    fn render_cells(&self, area: Rect, surface: &mut Surface, cx: &mut Context) {
        let term = self.term.lock();
        let content = term.renderable_content();

        let bg_style = cx.editor.theme.get("ui.background");
        surface.clear_with(area, bg_style);

        for indexed in content.display_iter {
            let point = indexed.point;
            let cell: &Cell = &indexed.cell;

            let col = point.column.0 as u16;
            let line = point.line.0;
            if line < 0 {
                continue;
            }
            let line = line as u16;

            let x = area.x + col;
            let y = area.y + line;

            if x >= area.right() || y >= area.bottom() {
                continue;
            }

            // Skip wide char spacers
            if cell.flags.contains(CellFlags::WIDE_CHAR_SPACER)
                || cell.flags.contains(CellFlags::LEADING_WIDE_CHAR_SPACER)
            {
                continue;
            }

            let fg = Self::convert_color(&cell.fg, content.colors);
            let bg = Self::convert_color(&cell.bg, content.colors);

            let mut modifier = Modifier::empty();
            if cell.flags.contains(CellFlags::BOLD) {
                modifier |= Modifier::BOLD;
            }
            if cell.flags.contains(CellFlags::ITALIC) {
                modifier |= Modifier::ITALIC;
            }
            // Underline is handled via UnderlineStyle on the Style, not as a Modifier
            if cell.flags.contains(CellFlags::DIM) {
                modifier |= Modifier::DIM;
            }
            if cell.flags.contains(CellFlags::STRIKEOUT) {
                modifier |= Modifier::CROSSED_OUT;
            }

            let (fg, bg) = if cell.flags.contains(CellFlags::INVERSE) {
                (bg, fg)
            } else {
                (fg, bg)
            };

            let mut style = Style::default().fg(fg).bg(bg).add_modifier(modifier);
            if cell.flags.contains(CellFlags::UNDERLINE) {
                style = style.underline_style(helix_view::graphics::UnderlineStyle::Line);
            }

            let ch = cell.c;
            if ch != ' ' && ch != '\0' {
                surface.set_string(x, y, &ch.to_string(), style);
            } else {
                surface.set_style(Rect::new(x, y, 1, 1), style);
            }
        }
    }
}

impl Component for TerminalPane {
    fn handle_event(&mut self, event: &Event, _ctx: &mut Context) -> EventResult {
        if !self.focus {
            return EventResult::Ignored(None);
        }

        match event {
            Event::Key(key) => {
                if let Some(bytes) = Self::key_to_bytes(key) {
                    self.write_to_pty(&bytes);
                }
                EventResult::Consumed(None)
            }
            Event::Paste(contents) => {
                self.write_to_pty(contents.as_bytes());
                EventResult::Consumed(None)
            }
            Event::Resize(_, _) | Event::FocusGained | Event::FocusLost | Event::IdleTimeout => {
                EventResult::Consumed(None)
            }
            Event::Mouse(_) => EventResult::Ignored(None),
        }
    }

    fn render(&mut self, area: Rect, surface: &mut Surface, cx: &mut Context) {
        if !self.open || area.width < 2 || area.height < 2 {
            return;
        }

        self.resize(area.height, area.width);
        self.render_cells(area, surface, cx);
    }

    fn cursor(&self, area: Rect, _editor: &Editor) -> (Option<Position>, CursorKind) {
        if !self.focus || !self.open {
            return (None, CursorKind::Hidden);
        }

        let term = self.term.lock();
        let content = term.renderable_content();
        let cursor = &content.cursor;

        if cursor.shape == CursorShape::Hidden {
            return (None, CursorKind::Hidden);
        }

        let line = cursor.point.line.0;
        let col = cursor.point.column.0;

        if line < 0 || line as u16 >= area.height || col >= area.width as usize {
            return (None, CursorKind::Hidden);
        }

        let x = area.x as usize + col;
        let y = area.y as usize + line as usize;

        let kind = match cursor.shape {
            CursorShape::Block => CursorKind::Block,
            CursorShape::HollowBlock => CursorKind::Block,
            CursorShape::Underline => CursorKind::Underline,
            CursorShape::Beam => CursorKind::Bar,
            CursorShape::Hidden => CursorKind::Hidden,
        };

        (Some(Position::new(y, x)), kind)
    }
}
