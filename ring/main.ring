Load "libsdl.ring"

if SDL_Init(SDL_INIT_EVERYTHING) < 0
   see "Init Error:" + SDL_GetError()
   exit(-1)
end

// Create the window
win = SDL_CreateWindow("Hello World!", 100, 100, 620, 387, SDL_WINDOW_SHOWN)

// Create a renderer
ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED|SDL_RENDERER_PRESENTVSYNC)

// Load the image
bmp = SDL_LoadBMP("../img/grumpy-cat.bmp")

// Use the image as a texture
tex = SDL_CreateTextureFromSurface(ren,bmp)

// No need for the image data after the texture has been created
SDL_FreeSurface(bmp)

for i = 0 to 20
    // Clear the renderer and display the image/texture
    SDL_RenderClear(ren)
    SDL_RenderCopy(ren, tex, NULL, NULL)
    SDL_RenderPresent(ren)

    // Wait 100 ms
    SDL_Delay(100)
next

SDL_DestroyTexture(tex)
SDL_DestroyRenderer(ren)
SDL_DestroyWindow(win)
SDL_Quit()
