use helix_view::graphics::{Color, Rect};
use tui::buffer::Buffer as Surface;

/// Render an image using Unicode half-block characters (fallback mode).
///
/// Each terminal cell displays two vertical pixels using the upper half block
/// character (U+2580) with fg = top pixel color and bg = bottom pixel color.
pub fn render_image_halfblock(
    surface: &mut Surface,
    viewport: Rect,
    image_bytes: &[u8],
    cell_pixel_size: (u16, u16),
) {
    let img = match image::load_from_memory(image_bytes) {
        Ok(img) => img,
        Err(_) => {
            let msg = "[Cannot decode image]";
            if viewport.width as usize >= msg.len() && viewport.height > 0 {
                for (i, ch) in msg.chars().enumerate() {
                    if let Some(cell) = surface.get_mut(viewport.x + i as u16, viewport.y) {
                        cell.set_char(ch);
                    }
                }
            }
            return;
        }
    };

    let (cell_w, cell_h) = cell_pixel_size;
    // Each halfblock cell is cell_w pixels wide and cell_h/2 pixels tall per "pixel"
    // To map image pixels to cells preserving aspect ratio, we compute the
    // "pixel aspect" of a halfblock pixel: (cell_w) / (cell_h / 2) = 2*cell_w/cell_h
    let halfblock_pixel_aspect = (2.0 * cell_w as f64) / cell_h as f64;

    // Available cell grid (use at most 80% of viewport)
    let grid_w = viewport.width as u32;
    let grid_h = viewport.height as u32 * 2; // 2 halfblock pixels per cell row

    if grid_w == 0 || grid_h == 0 {
        return;
    }

    let img_aspect = img.width() as f64 / img.height() as f64;

    // Image's natural size in halfblock grid pixels (don't upscale)
    // Each halfblock pixel = cell_w physical px wide, cell_h/2 physical px tall
    let nat_w = (img.width() as f64 / cell_w as f64).ceil().max(1.0) as u32;
    let nat_h = (img.height() as f64 / (cell_h as f64 / 2.0)).ceil().max(1.0) as u32;

    let max_w = ((grid_w as f64 * 0.8).floor().max(1.0) as u32).min(nat_w);
    let max_h = ((grid_h as f64 * 0.8).floor().max(1.0) as u32).min(nat_h);

    let vp_phys_w = max_w as f64 * halfblock_pixel_aspect;
    let vp_phys_h = max_h as f64;
    let vp_aspect = vp_phys_w / vp_phys_h;

    let (target_w, target_h) = if img_aspect > vp_aspect {
        let tw = max_w;
        let th = ((tw as f64 * halfblock_pixel_aspect) / img_aspect).round().max(1.0) as u32;
        (tw, th.min(max_h))
    } else {
        let th = max_h;
        let tw = ((th as f64 * img_aspect) / halfblock_pixel_aspect).round().max(1.0) as u32;
        (tw.min(max_w), th)
    };

    let img = img.resize_exact(target_w, target_h, image::imageops::FilterType::Triangle);
    let rgba = img.to_rgba8();
    let (img_w, img_h) = rgba.dimensions();

    // Center the image in the viewport
    let x_offset = grid_w.saturating_sub(img_w) / 2;
    let y_pixel_offset = grid_h.saturating_sub(img_h) / 2;
    let y_cell_offset = y_pixel_offset / 2;

    for cy in 0..viewport.height {
        for cx in 0..viewport.width {
            let img_x = (cx as u32).checked_sub(x_offset);
            let img_top_y = (cy as u32 * 2).checked_sub(y_cell_offset * 2);

            let (img_x, img_top_y) = match (img_x, img_top_y) {
                (Some(x), Some(y)) if x < img_w && y < img_h => (x, y),
                _ => continue,
            };

            let top_pixel = rgba.get_pixel(img_x, img_top_y);
            let fg = Color::Rgb(top_pixel[0], top_pixel[1], top_pixel[2]);

            let img_bot_y = img_top_y + 1;
            let bg = if img_bot_y < img_h {
                let bottom_pixel = rgba.get_pixel(img_x, img_bot_y);
                Color::Rgb(bottom_pixel[0], bottom_pixel[1], bottom_pixel[2])
            } else {
                Color::Reset
            };

            if let Some(cell) = surface.get_mut(viewport.x + cx, viewport.y + cy) {
                cell.set_symbol("\u{2580}"); // upper half block
                cell.set_fg(fg);
                cell.set_bg(bg);
            }
        }
    }
}

/// Result of encoding an image for terminal protocol rendering.
pub struct EncodedImage {
    pub png_bytes: Vec<u8>,
    /// Actual number of columns the image should span.
    pub cols: u16,
    /// Actual number of rows the image should span.
    pub rows: u16,
    /// X offset to center the image in the viewport.
    pub x_offset: u16,
    /// Y offset to center the image in the viewport.
    pub y_offset: u16,
}

/// Resize an image to fit a terminal viewport (preserving aspect ratio)
/// and encode it as PNG for terminal graphics protocol rendering.
pub fn encode_png_for_viewport(
    image_bytes: &[u8],
    viewport_width: u16,
    viewport_height: u16,
    cell_pixel_size: (u16, u16),
) -> Option<EncodedImage> {
    use image::ImageEncoder;

    let img = image::load_from_memory(image_bytes).ok()?;
    let (orig_w, orig_h) = (img.width(), img.height());

    if orig_w == 0 || orig_h == 0 || viewport_width == 0 || viewport_height == 0 {
        return None;
    }

    let (cell_pw, cell_ph) = cell_pixel_size;
    let cell_aspect = cell_pw as f64 / cell_ph as f64;
    let img_aspect = orig_w as f64 / orig_h as f64;

    // Image's natural size in cells (don't upscale beyond original resolution)
    let nat_cols = (orig_w as f64 / cell_pw as f64).ceil().max(1.0) as u16;
    let nat_rows = (orig_h as f64 / cell_ph as f64).ceil().max(1.0) as u16;

    // Use at most 80% of the viewport, and no larger than the image's natural size
    let max_cols = (viewport_width as f64 * 0.8).floor().max(1.0) as u16;
    let max_cols = max_cols.min(nat_cols);
    let max_rows = (viewport_height as f64 * 0.8).floor().max(1.0) as u16;
    let max_rows = max_rows.min(nat_rows);

    let vp_aspect = max_cols as f64 * cell_aspect / max_rows as f64;

    let (cols, rows) = if img_aspect > vp_aspect {
        let cols = max_cols;
        let rows = ((cols as f64 * cell_aspect) / img_aspect).round().max(1.0) as u16;
        (cols, rows.min(max_rows))
    } else {
        let rows = max_rows;
        let cols = ((rows as f64 * img_aspect) / cell_aspect).round().max(1.0) as u16;
        (cols.min(max_cols), rows)
    };

    // Resize to actual pixel resolution for the cells it will occupy
    let pixel_w = cols as u32 * cell_pw as u32;
    let pixel_h = rows as u32 * cell_ph as u32;

    let resized = img.resize_exact(pixel_w, pixel_h, image::imageops::FilterType::Triangle);
    let rgba = resized.to_rgba8();
    let (w, h) = rgba.dimensions();

    let mut png_buf = Vec::new();
    let encoder = image::codecs::png::PngEncoder::new(&mut png_buf);
    encoder
        .write_image(rgba.as_raw(), w, h, image::ExtendedColorType::Rgba8)
        .ok()?;

    let x_offset = viewport_width.saturating_sub(cols) / 2;
    let y_offset = viewport_height.saturating_sub(rows) / 2;

    Some(EncodedImage {
        png_bytes: png_buf,
        cols,
        rows,
        x_offset,
        y_offset,
    })
}
