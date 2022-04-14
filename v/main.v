module main

import sdl

fn main() {
	if sdl.init(sdl.init_everything) != 0 {
		error_msg := unsafe { cstring_to_vstring(sdl.get_error()) }
		panic('sdl.init error:\n$error_msg')
	}

	window := sdl.create_window(c'Hello World!', 100, 100, 620, 387, u32(sdl.WindowFlags.shown))
	if window == sdl.null {
		error_msg := unsafe { cstring_to_vstring(sdl.get_error()) }
		panic('sdl.create_window error:\n$error_msg')
	}

	renderer := sdl.create_renderer(window, -1, u32(sdl.RendererFlags.accelerated) | u32(sdl.RendererFlags.presentvsync))
	if renderer == sdl.null {
		sdl.destroy_window(window)
		sdl.quit()
		error_msg := unsafe { cstring_to_vstring(sdl.get_error()) }
		panic('sdl.create_renderer error:\n$error_msg')
	}

	bmp := sdl.load_bmp(c'../img/grumpy-cat.bmp')
	if bmp == sdl.null {
		error_msg := unsafe { cstring_to_vstring(sdl.get_error()) }
		panic('sdl.load_bmp error:\n$error_msg')
	}

	texture := sdl.create_texture_from_surface(renderer, bmp)
	if texture == sdl.null {
		sdl.free_surface(bmp)
		sdl.destroy_renderer(renderer)
		sdl.destroy_window(window)
		sdl.quit()
		error_msg := unsafe { cstring_to_vstring(sdl.get_error()) }
		panic('sdl.create_texture_from_surface error:\n$error_msg')
	}
	sdl.free_surface(bmp)

	for i := 0; i < 20; i++ {
		sdl.render_clear(renderer)
		sdl.render_copy(renderer, texture, sdl.null, sdl.null)
		sdl.render_present(renderer)
		sdl.delay(100)
	}

	sdl.destroy_texture(texture)
	sdl.destroy_renderer(renderer)
	sdl.destroy_window(window)
	sdl.quit()
}
