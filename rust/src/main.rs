extern crate sdl2;

use std::path::Path;
use sdl2::surface::{Surface};

fn main() {
    let ctx = sdl2::init().unwrap();
    let video_ctx = ctx.video().unwrap();
    let mut timer = ctx.timer().unwrap();

    // Create a new OpenGL window
    let mut window  = match video_ctx.window("Hello World!", 960, 540).position_centered().opengl().build() {
        Ok(window) => window,
        Err(err)   => panic!("failed to create window: {}", err)
    };

    // Load the image as a surface
    let surface = match Surface::load_bmp(&Path::new("../img/boxes.bmp")) {
        Ok(surface) => surface,
        Err(err)    => panic!("failed to load surface: {}", err)
    };

    {
        {
            // Get the window surface
            let events = ctx.event_pump().unwrap();
            let screen = window.surface_mut(&events).unwrap();

            // Blit the image to the window
            unsafe {
                let _ = surface.blit(None, screen, None);
            }
        }

        for _ in 0..20 {

            // Update the window to display the changed surface
            match window.update_surface() {
                Ok(_) => {},
                Err(err) => panic!("failed to update window surface: {}", err)
            }

            // Wait 100ms
            timer.delay(100);
        }
    }

}
