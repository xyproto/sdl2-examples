#include <stdio.h>
#include <SDL2/SDL.h>

int main() {
  void* nullptr = NULL;

  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
    printf("SDL_Init Error: %s\n", SDL_GetError());
    return 1;
  }

  SDL_Window *win = SDL_CreateWindow("Hello World!", 100, 100, 960, 540, SDL_WINDOW_SHOWN);
  if (win == nullptr) {
    printf("SDL_CreateWindow Error: %s\n", SDL_GetError());
    return 1;
  }

  SDL_Renderer *ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (ren == nullptr) {
    printf("SDL_CreateRenderer Error: %s\n", SDL_GetError());
    return 1;
  }

  SDL_Surface *bmp = SDL_LoadBMP("../img/boxes.bmp");
  if (bmp == nullptr) {
    printf("SDL_LoadBMP Error: %s\n", SDL_GetError());
    return 1;
  }

  SDL_Texture *tex = SDL_CreateTextureFromSurface(ren, bmp);
  SDL_FreeSurface(bmp);
  if (tex == nullptr) {
    printf("SDL_CreateTextureFromSurface Error: %s\n", SDL_GetError());
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
