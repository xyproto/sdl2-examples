package main

import (
  "log"
  "github.com/veandco/go-sdl2/sdl"
)

// This file is more similar to the C and C++ examples than strictly needed

func main() {
	if sdl.Init(sdl.INIT_EVERYTHING) != 0 {
		log.Fatalf("SDL_Init Error: %s\n", sdl.GetError())
	}

	// Prepare the window and load the image
	win := sdl.CreateWindow("Hello World!", 100, 100, 640, 480, sdl.WINDOW_SHOWN)
	ren := sdl.CreateRenderer(win, -1, sdl.RENDERER_ACCELERATED | sdl.RENDERER_PRESENTVSYNC);
	bmp := sdl.LoadBMP("test.bmp")
	tex := ren.CreateTextureFromSurface(bmp)

	// Show the image
	ren.Clear()
	ren.Copy(tex, nil, nil)
	ren.Present()

	// Wait 2 seconds
	sdl.Delay(2000)

	// These three are not strictly needed
	tex.Destroy()
	ren.Destroy()
	win.Destroy()

	// Neither is this one
	sdl.Quit()
}
