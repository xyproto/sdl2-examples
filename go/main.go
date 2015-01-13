package main

import (
	"log"

	"github.com/veandco/go-sdl2/sdl"
)

func main() {
	// Initialize
	if sdl.Init(sdl.INIT_EVERYTHING) != 0 {
		log.Fatalf("Init Error: %s\n", sdl.GetError())
	}
	// Make sure to quit when the function returns
	defer sdl.Quit()

	// Create the window
	win, err := sdl.CreateWindow("Hello World!", 100, 100, 960, 540, sdl.WINDOW_SHOWN)
	if err != nil {
		log.Fatalf("CreateWindow Error: %s\n", err.Error())
	}
	defer win.Destroy()

	// Create a renderer
	ren, err := sdl.CreateRenderer(win, -1, sdl.RENDERER_ACCELERATED|sdl.RENDERER_PRESENTVSYNC)
	if err != nil {
		log.Fatalf("CreateRenderer Error: %s\n", err.Error())
	}
	defer ren.Destroy()

	// Load the image
	bmp := sdl.LoadBMP("../img/boxes.bmp")
	if bmp == nil {
		log.Fatalf("LoadBMP Error: %s\n", sdl.GetError())
	}

	// Use the image as a texture
	tex, err := ren.CreateTextureFromSurface(bmp)
	if err != nil {
		log.Fatalf("CreateTextureFromSurface Error: %s\n", err.Error())
	}
	defer tex.Destroy()

	// No need for the image data after the texture has been created
	bmp.Free()

	// Clear the renderer and display the image/texture
	ren.Clear()
	ren.Copy(tex, nil, nil)
	ren.Present()

	// Wait 2 seconds
	sdl.Delay(2000)
}
