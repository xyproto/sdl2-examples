extern crate sdl2;

use sdl2::surface::Surface;
use std::path::Path;

fn main() {
    let ctx = sdl2::init().unwrap();
    let video_subsystem = ctx.video().unwrap();
    let mut timer = ctx.timer().unwrap();

    // Create a new OpenGL window
    let window = match video_subsystem
        .window("Hello World!", 620, 387)
        .position_centered()
        .opengl()
        .build()
    {
        Ok(window) => window,
        Err(err) => panic!("failed to create window: {}", err),
    };

    // Load the image as a surface
    let surface = match Surface::load_bmp(&Path::new("../img/grumpy-cat.bmp")) {
        Ok(surface) => surface,
        Err(err) => panic!("failed to load surface: {}", err),
    };

    // Get the window canvas
    let event_pump = ctx.event_pump().unwrap();
    let mut screen = window.surface(&event_pump).unwrap();

    // Blit the image to the window
    surface.blit(None, &mut screen, None).unwrap();

    for _ in 0..20 {
        // Update the window to display the changed surface
        if let Err(err) = screen.update_window() {
            panic!("failed to update window surface: {}", err);
        }

        // Wait 100ms
        timer.delay(100);
    }
}
