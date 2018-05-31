#include <SDL2/SDL.h>
#include <iostream>

#include "sdl2.h"

auto main() -> int
{
    using std::cout;
    using std::endl;

    auto sys = sdl2::make_sdlsystem(SDL_INIT_EVERYTHING);
    if (!sys) {
        cout << "Error creating SDL2 system: " << SDL_GetError() << endl;
        return 1;
    }

    auto win = sdl2::make_window("Hello World!", 100, 100, 960, 540, SDL_WINDOW_SHOWN);
    if (!win) {
        cout << "Error creating window: " << SDL_GetError() << endl;
        return 1;
    }

    auto ren
        = sdl2::make_renderer(win.get(), -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!ren) {
        cout << "Error creating renderer: " << SDL_GetError() << endl;
        return 1;
    }

    auto file = SDL_RWFromFile(IMGDIR "boxes.bmp", "rb");
    if (file == nullptr) {
        cout << "Error reading file: " << SDL_GetError() << endl;
        return 1;
    }

    auto bmp = sdl2::make_bmp(file);
    if (!bmp) {
        cout << "Error creating surface: " << SDL_GetError() << endl;
        return 1;
    }

    auto tex = sdl2::make_texture(ren.get(), bmp.get());
    if (!tex) {
        cout << "Error creating texture: " << SDL_GetError() << endl;
        return 1;
    }

    for (int i = 0; i < 20; i++) {
        SDL_RenderClear(ren.get());
        SDL_RenderCopy(ren.get(), tex.get(), nullptr, nullptr);
        SDL_RenderPresent(ren.get());
        SDL_Delay(100);
    }

    return 0;
}
