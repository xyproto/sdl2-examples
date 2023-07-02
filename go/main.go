package main

import (
	"log"
	"path/filepath"

	"github.com/veandco/go-sdl2/img"
	"github.com/veandco/go-sdl2/sdl"
)

func main() {
	// Initialize
	if err := sdl.Init(sdl.INIT_EVERYTHING); err != nil {
		log.Fatalln("Init Error:", err)
	}
	// Make sure to quit when the function returns
	defer sdl.Quit()

	// Create the window
	win, err := sdl.CreateWindow("Hello World!", 100, 100, 620, 387, sdl.WINDOW_SHOWN)
	if err != nil {
		log.Fatalln("CreateWindow Error:", err)
	}
	defer win.Destroy()

	// Create a renderer
	ren, err := sdl.CreateRenderer(win, -1, sdl.RENDERER_ACCELERATED|sdl.RENDERER_PRESENTVSYNC)
	if err != nil {
		log.Fatalln("CreateRenderer Error:", err)
	}
	defer ren.Destroy()

	// Load the image as a texture
	tex, err := img.LoadTexture(ren, filepath.Join("..", "img", "grumpy-cat.png"))
	if err != nil {
		log.Fatalln("LoadTexture Error:", err)
	}
	defer tex.Destroy()

	for i := 0; i < 20; i++ {
		// Clear the renderer and display the image/texture
		ren.Clear()
		ren.Copy(tex, nil, nil)
		ren.Present()

		// Wait 100 ms
		sdl.Delay(100)
	}
}
