extern crate sdl2;
extern crate native;

use sdl2::timer;
use sdl2::render::Renderer;
use sdl2::render;
use sdl2::surface::Surface;
use sdl2::video::Window;
use sdl2::video;

fn main() {
    sdl2::init(sdl2::InitEverything);

    let win = Window::new("Hello World!",
                          video::Positioned(100),
                          video::Positioned(100),
                          960, 540,
                          video::Shown).unwrap();

    let ren = Renderer::from_window(win,
                                    render::DriverAuto,
                                    render::Accelerated).unwrap();

    let bmp = Surface::from_bmp(&Path::new("../img/boxes.bmp")).unwrap();

    let tex = ren.create_texture_from_surface(&bmp).unwrap();

    let _ = ren.clear();
    let _ = ren.copy(&tex, None, None);
    ren.present();

    timer::delay(2000);

    sdl2::quit();
}
