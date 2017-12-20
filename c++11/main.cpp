#include <SDL2/SDL.h>
#include <iostream>

using std::cout;
using std::endl;

int main() {
  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
      cout << "SDL_Init Error: " << SDL_GetError() << endl;
      return 1;
  }

  SDL_Window *win = SDL_CreateWindow("Hello World!", 100, 100, 960, 540, SDL_WINDOW_SHOWN);
  if (win == nullptr) {
      cout << "SDL_CreateWindow Error: " << SDL_GetError() << endl;
      return 1;
  }

  SDL_Renderer *ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (ren == nullptr) {
      cout << "SDL_CreateRenderer Error" << SDL_GetError() << endl;
      return 1;
  }

  SDL_Surface *bmp = SDL_LoadBMP("../img/boxes.bmp");
  if (bmp == nullptr) {
      cout << "SDL_LoadBMP Error: " << SDL_GetError() << endl;
      return 1;
  }

  SDL_Texture *tex = SDL_CreateTextureFromSurface(ren, bmp);
  SDL_FreeSurface(bmp);
  if (tex == nullptr) {
      cout << "SDL_CreateTextureFromSurface Error: " << SDL_GetError() << endl;
      return 1;
  }

  for (int i=0; i < 20; i++) {
      SDL_RenderClear(ren);
      SDL_RenderCopy(ren, tex, nullptr, nullptr);
      SDL_RenderPresent(ren);
      SDL_Delay(100);
  }

  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(ren);
  SDL_DestroyWindow(win);
  SDL_Quit();

  return 0;
}
