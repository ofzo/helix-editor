//! Provides interface for controlling the terminal

use std::io;

use crate::{buffer::Cell, terminal::Config};

use helix_view::{
    graphics::{CursorKind, Rect},
    theme::Color,
};

#[cfg(all(feature = "termina", not(windows)))]
mod termina;
#[cfg(all(feature = "termina", not(windows)))]
pub use self::termina::TerminaBackend;

#[cfg(all(feature = "termina", windows))]
mod crossterm;
#[cfg(all(feature = "termina", windows))]
pub use self::crossterm::CrosstermBackend;

mod test;
pub use self::test::TestBackend;

/// Supported terminal graphics protocols for inline image display.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GraphicsProtocol {
    /// Kitty graphics protocol — supported by Kitty, WezTerm, Ghostty.
    Kitty,
    /// iTerm2 inline images (OSC 1337) — supported by iTerm2, WezTerm.
    Iterm2,
}

/// Representation of a terminal backend.
pub trait Backend {
    /// Claims the terminal for TUI use.
    fn claim(&mut self) -> Result<(), io::Error>;
    /// Update terminal configuration.
    fn reconfigure(&mut self, config: Config) -> Result<(), io::Error>;
    /// Restores the terminal to a normal state, undoes `claim`
    fn restore(&mut self) -> Result<(), io::Error>;
    /// Draws styled text to the terminal
    fn draw<'a, I>(&mut self, content: I) -> Result<(), io::Error>
    where
        I: Iterator<Item = (u16, u16, &'a Cell)>;
    /// Hides the cursor
    fn hide_cursor(&mut self) -> Result<(), io::Error>;
    /// Sets the cursor to the given shape
    fn show_cursor(&mut self, kind: CursorKind) -> Result<(), io::Error>;
    /// Sets the cursor to the given position
    fn set_cursor(&mut self, x: u16, y: u16) -> Result<(), io::Error>;
    /// Clears the terminal
    fn clear(&mut self) -> Result<(), io::Error>;
    /// Gets the size of the terminal in cells
    fn size(&self) -> Result<Rect, io::Error>;
    /// Flushes the terminal buffer
    fn flush(&mut self) -> Result<(), io::Error>;
    fn supports_true_color(&self) -> bool;
    fn get_theme_mode(&self) -> Option<helix_view::theme::Mode>;
    fn set_background_color(&mut self, color: Option<Color>) -> io::Result<()>;

    /// Returns the terminal graphics protocol supported by this backend, if any.
    fn graphics_protocol(&self) -> Option<GraphicsProtocol> {
        None
    }

    /// Returns the pixel dimensions of a single terminal cell (width, height).
    /// Falls back to (8, 16) if the terminal doesn't report pixel size.
    fn cell_pixel_size(&self) -> (u16, u16) {
        (8, 16)
    }

    /// Clear all previously rendered terminal protocol images.
    fn clear_images(&mut self) -> Result<(), io::Error> {
        Ok(())
    }

    /// Render a PNG image at the given cell position spanning width x height cells.
    /// The image_data should be PNG-encoded bytes.
    fn draw_image(
        &mut self,
        _x: u16,
        _y: u16,
        _width: u16,
        _height: u16,
        _image_png: &[u8],
    ) -> Result<(), io::Error> {
        Ok(())
    }
}
