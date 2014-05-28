package main

import (
	"github.com/veandco/go-sdl2/sdl"
	"log"
)

// Note that the SDL2 package could have been more ideomatic by
// returning error values that could contain error messages instead
// of having to use "if == nil", "sdl.GetError()" and the .Destroy() functions.

func main() {
	// Initialize
	if sdl.Init(sdl.INIT_EVERYTHING) != 0 {
		log.Fatalf("Init Error: %s\n", sdl.GetError())
	}
	// Make sure to quit when the function returns
	defer sdl.Quit()

	// Create the window
	win := sdl.CreateWindow("Hello World!", 100, 100, 960, 540, sdl.WINDOW_SHOWN)
	if win == nil {
		log.Fatalf("CreateWindow Error: %s\n", sdl.GetError())
	}
	defer win.Destroy()

	// Create a renderer
	ren := sdl.CreateRenderer(win, -1, sdl.RENDERER_ACCELERATED|sdl.RENDERER_PRESENTVSYNC)
	if win == nil {
		log.Fatalf("CreateRenderer Error: %s\n", sdl.GetError())
	}
	defer ren.Destroy()

	// Load the image
	bmp := sdl.LoadBMP("../img/boxes.bmp")
	if bmp == nil {
		log.Fatalf("LoadBMP Error: %s\n", sdl.GetError())
	}

	// Use the image as a texture
	tex := ren.CreateTextureFromSurface(bmp)
	if tex == nil {
		log.Fatalf("CreateTextureFromSurface Error: %s\n", sdl.GetError())
	}
	defer tex.Destroy()

	// Show the texture
	ren.Clear()
	ren.Copy(tex, nil, nil)
	ren.Present()

	// Wait 2 seconds
	sdl.Delay(2000)
}
