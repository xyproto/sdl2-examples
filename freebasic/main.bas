#include "SDL2/SDL.bi"
#include "SDL2/SDL_image.bi"
#include "SDL2/SDL_ttf.bi"

SDL_Init(SDL_INIT_VIDEO)
IMG_Init(0)

var win = SDL_CreateWindow("Hello world!", 100, 100, 620, 387, 0)
var ren = SDL_CreateRenderer(win, -1, 0)

'' Load the image into a surface and then into a texture
var img = IMG_Load("../img/grumpy-cat.bmp")
var tex = SDL_CreateTextureFromSurface(ren, img)
SDL_FreeSurface(img) : img = NULL

for i as Integer = 0 to 20
  SDL_RenderClear(ren)
  SDL_RenderCopy(ren, tex, NULL, NULL)
  SDL_RenderPresent(ren)
  SDL_Delay(100)
next i

SDL_DestroyTexture(tex)
SDL_DestroyRenderer(ren)
SDL_DestroyWindow(win)
SDL_Quit()
