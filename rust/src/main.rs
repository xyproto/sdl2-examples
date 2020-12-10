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

    // Get the window surface, and prepare to handle events
    let event_pump = ctx.event_pump().unwrap();
    let mut window_surf = window.surface(&event_pump).unwrap();

    // Blit the image to the window surface
    surface.blit(None, &mut window_surf, None).unwrap();

    for _ in 0..20 {
        // Display the contents of the window surface on the screen
        if let Err(err) = window_surf.update_window() {
            panic!("failed to update window surface: {}", err);
        }

        // Wait 100ms
        timer.delay(100);
    }
}
