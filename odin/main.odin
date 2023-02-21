package main

import "core:log"
import "vendor:sdl2"

main :: proc() {
    // Initialize
    if err := sdl2.Init(sdl2.INIT_EVERYTHING); err < 0 {
	log.fatal("Init Error:", sdl2.GetError())
    }
    // Make sure to quit when the function returns
    defer sdl2.Quit()

    // Create the window
    win := sdl2.CreateWindow("Hello World!", 100, 100, 620, 387, sdl2.WINDOW_SHOWN)
    defer sdl2.DestroyWindow(win)

    // Create a renderer
    ren := sdl2.CreateRenderer(win, -1, sdl2.RENDERER_ACCELERATED|sdl2.RENDERER_PRESENTVSYNC)
    defer sdl2.DestroyRenderer(ren)

    // Load the image
    bmp := sdl2.LoadBMP("../img/grumpy-cat.bmp")

    // Use the image as a texture
    tex := sdl2.CreateTextureFromSurface(ren,bmp)
    defer sdl2.DestroyTexture(tex)

    // No need for the image data after the texture has been created
    sdl2.FreeSurface(bmp)

    for i := 0; i < 20; i += 1 {
	// Clear the renderer and display the image/texture
	sdl2.RenderClear(ren)
	sdl2.RenderCopy(ren, tex, nil, nil)
	sdl2.RenderPresent(ren)

	// Wait 100 ms
	sdl2.Delay(100)
    }
}
