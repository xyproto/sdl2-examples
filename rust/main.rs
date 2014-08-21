extern crate sdl2;
extern crate native;

fn main() {
    sdl2::init(sdl2::InitEverything);

    let win = match sdl2::video::Window::new("Hello World!", sdl2::video::Positioned(100), sdl2::video::Positioned(100), 960, 540, sdl2::video::Shown) {
        Ok(window) => window,
        Err(err) => fail!(format!("SDL_CreateWindow Error: {}", err))
    };

    let ren = match sdl2::render::Renderer::from_window(win, sdl2::render::DriverAuto, sdl2::render::Accelerated) {
        Ok(renderer) => renderer,
        Err(err) => fail!(format!("SDL_CreateRenderer Error: {}", err))
    };

    let bmp = match sdl2::surface::Surface::from_bmp(&Path::new("../img/boxes.bmp")) {
    	Ok(bmp) => bmp,
	Err(err) => fail!(format!("SDL_LoadBMP Error: {}", err))
    };

    let tex = match ren.create_texture_from_surface(&bmp) {
    	Ok(tex) => tex,
	Err(err) => fail!(format!("SDL_CreateTextureFromSurface Error: {}", err))
    };

    let _ = ren.clear();
    let _ = ren.copy(&tex, None, None);
    ren.present();

    sdl2::timer::delay(2000);

    sdl2::quit();
}
