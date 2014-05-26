package main

import (
	"github.com/veandco/go-sdl2/sdl"
	"log"
)

func main() {
	// Initialize
	if sdl.Init(sdl.INIT_EVERYTHING) != 0 {
		log.Fatalf("SDL_Init Error: %s\n", sdl.GetError())
	}
	// Make sure to quit when the function returns
	defer sdl.Quit()

	// Prepare the window and load the image
	win := sdl.CreateWindow("Hello World!", 100, 100, 960, 540, sdl.WINDOW_SHOWN)
	defer win.Destroy()

	// Prepare a renderer for the window
	ren := sdl.CreateRenderer(win, -1, sdl.RENDERER_ACCELERATED|sdl.RENDERER_PRESENTVSYNC)
	defer ren.Destroy()

	// Load the image and prepare to use it as a texture
	bmp := sdl.LoadBMP("../img/boxes.bmp")
	tex := ren.CreateTextureFromSurface(bmp)
	defer tex.Destroy()

	// Show the texture
	ren.Clear()
	ren.Copy(tex, nil, nil)
	ren.Present()

	// Wait 2 seconds
	sdl.Delay(2000)
}
