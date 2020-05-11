#include <SDL2/SDL.h>
#include <cstdlib>
#include <iostream>

int main()
{
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        std::cerr << "SDL_Init Error: " << SDL_GetError() << std::endl;
        return EXIT_FAILURE;
    }

    SDL_Window* win = SDL_CreateWindow("Hello World!", 100, 100, 620, 387, SDL_WINDOW_SHOWN);
    if (win == NULL) {
        std::cerr << "SDL_CreateWindow Error: " << SDL_GetError() << std::endl;
        return EXIT_FAILURE;
    }

    SDL_Renderer* ren
        = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (ren == NULL) {
        std::cerr << "SDL_CreateRenderer Error: " << SDL_GetError() << std::endl;
		if (win != NULL) {
			SDL_DestroyWindow(win);
		}
		SDL_Quit();
        return EXIT_FAILURE;
    }

    SDL_Surface* bmp = SDL_LoadBMP("../img/grumpy-cat.bmp");
    if (bmp == NULL) {
        std::cerr << "SDL_LoadBMP Error: " << SDL_GetError() << std::endl;
		if (ren != NULL) {
			SDL_DestroyRenderer(ren);
		}
		if (win != NULL) {
			SDL_DestroyWindow(win);
		}
		SDL_Quit();
        return EXIT_FAILURE;
    }

    SDL_Texture* tex = SDL_CreateTextureFromSurface(ren, bmp);
    if (tex == NULL) {
        std::cerr << "SDL_CreateTextureFromSurface Error: " << SDL_GetError() << std::endl;
		if (bmp != NULL) {
			SDL_FreeSurface(bmp);
		}
		if (ren != NULL) {
			SDL_DestroyRenderer(ren);
		}
		if (win != NULL) {
			SDL_DestroyWindow(win);
		}
		SDL_Quit();
        return EXIT_FAILURE;
    }
	SDL_FreeSurface(bmp);

    for (int i = 0; i < 20; i++) {
        SDL_RenderClear(ren);
        SDL_RenderCopy(ren, tex, NULL, NULL);
        SDL_RenderPresent(ren);
        SDL_Delay(100);
    }

    SDL_DestroyTexture(tex);
    SDL_DestroyRenderer(ren);
    SDL_DestroyWindow(win);
    SDL_Quit();

    return EXIT_SUCCESS;
}
