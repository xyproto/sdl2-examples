#include <SDL2/SDL.h>
#include <iostream>

int main() {
  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
    std::cout << "SDL_Init Error: " << SDL_GetError() << std::endl;
    return 1;
  }

  SDL_Window *win = SDL_CreateWindow("Hello World!", 100, 100, 620, 387, SDL_WINDOW_SHOWN);
  if (win == NULL) {
    std::cout << "SDL_CreateWindow Error: " << SDL_GetError() << std::endl;
    return 1;
  }

  SDL_Renderer *ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (ren == NULL) {
    std::cout << "SDL_CreateRenderer Error: " << SDL_GetError() << std::endl;
    return 1;
  }

  SDL_Surface *bmp = SDL_LoadBMP("../img/grumpy-cat.bmp");
  if (bmp == NULL) {
    std::cout << "SDL_LoadBMP Error: " << SDL_GetError() << std::endl;
    return 1;
  }

  SDL_Texture *tex = SDL_CreateTextureFromSurface(ren, bmp);
  SDL_FreeSurface(bmp);
  if (tex == NULL) {
    std::cout << "SDL_CreateTextureFromSurface Error: " << SDL_GetError() << std::endl;
    return 1;
  }

  for (int i=0; i < 20; i++) {
      SDL_RenderClear(ren);
      SDL_RenderCopy(ren, tex, NULL, NULL);
      SDL_RenderPresent(ren);
      SDL_Delay(100);
  }

  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(ren);
  SDL_DestroyWindow(win);
  SDL_Quit();

  return 0;
}
