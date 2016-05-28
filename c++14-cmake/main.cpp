#include <SDL2/SDL.h>
#include <memory>
#include <iostream>

using namespace std;

// Very useful function from Eric Scott Barr.
// https://eb2.co/blog/2014/04/c-plus-plus-14-and-sdl2-managing-resources/
template<typename Creator, typename Destructor, typename... Arguments>
auto make_resource(Creator c, Destructor d, Arguments&&... args)
{
  auto r = c(std::forward<Arguments>(args)...);
  return std::unique_ptr<std::decay_t<decltype(*r)>, decltype(d)>(r, d);
}

namespace sdl2
{

  using window_ptr_t = std::unique_ptr<SDL_Window, decltype(&SDL_DestroyWindow)>;
  using renderer_ptr_t = std::unique_ptr<SDL_Renderer, decltype(&SDL_DestroyRenderer)>;
  using surf_ptr_t = std::unique_ptr<SDL_Surface, decltype(&SDL_FreeSurface)>;
  using texture_ptr_t = std::unique_ptr<SDL_Texture, decltype(&SDL_DestroyTexture)>;

  // Create a windows (unique_ptr with both a window and the destructor)
  inline window_ptr_t make_window(const char* title, int x, int y, int w, int h, Uint32 flags) {
    return make_resource(SDL_CreateWindow, SDL_DestroyWindow, title, x, y, w, h, flags);
  }

  // Create a renderer given a window, containing both the renderer and the destructor
  inline renderer_ptr_t make_renderer(SDL_Window* win, int x, Uint32 flags) {
    return make_resource(SDL_CreateRenderer, SDL_DestroyRenderer, win, x, flags);
  }

  // Create a surface from a bmp file, containing both the surface and the destructor
  inline surf_ptr_t make_bmp(SDL_RWops* sdlfile) {
    // May throw an exception if sdlfile is nullptr
    return make_resource(SDL_LoadBMP_RW, SDL_FreeSurface, sdlfile, 1);
  }

  // Create a texture from a renderer and a surface
  inline texture_ptr_t make_texture(SDL_Renderer* ren, SDL_Surface* surf) {
    return make_resource(SDL_CreateTextureFromSurface, SDL_DestroyTexture, ren, surf);
  }

  class SDL2System {

    private:

      bool _initialized = false;
      const char* _error = nullptr;

    public:

      SDL2System() {
        if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
          _error = SDL_GetError();
          _initialized = false;
          return;
        }
        _initialized = true;
      }

      ~SDL2System() {
        SDL_Quit();
        _initialized = false;
        _error = nullptr;
      }

      bool isInitialized() {
        return _initialized;
      }

      const char* Error() {
        return _error;
      }

  };

}

int main() {
  sdl2::SDL2System sys;
  if (!sys.isInitialized()) {
    cout << "Error initializing SDL2: " << sys.Error() << endl;
    return 1;
  }

  auto win = sdl2::make_window("Hello World!", 100, 100, 960, 540, SDL_WINDOW_SHOWN);
  if (win.get() == nullptr) {
    cout << "Error creating window: " << SDL_GetError() << endl;
    return 1;
  }

  auto ren = sdl2::make_renderer(win.get(), -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (ren.get() == nullptr) {
    cout << "Error creating renderer: " << SDL_GetError() << endl;
    return 1;
  }

  auto file = SDL_RWFromFile("../img/boxes.bmp", "rb");
  if (file == nullptr) {
    cout << "Error reading file: " << SDL_GetError() << endl;
    return 1;
  }

  auto bmp = sdl2::make_bmp(file);
  if (bmp.get() == nullptr) {
    cout << "Error creating surface: " << SDL_GetError() << endl;
    return 1;
  }

  auto tex = sdl2::make_texture(ren.get(), bmp.get());
  if (tex.get() == nullptr) {
    cout << "Error creating texture: " << SDL_GetError() << endl;
    return 1;
  }

  for (int i=0; i < 20; i++) {
    SDL_RenderClear(ren.get());
    SDL_RenderCopy(ren.get(), tex.get(), NULL, NULL);
    SDL_RenderPresent(ren.get());
    SDL_Delay(100);
  }

  return 0;
}
