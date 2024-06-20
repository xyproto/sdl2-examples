#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int main(void)
{
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        fprintf(stderr, "SDL_Init Error: %s\n", SDL_GetError());
        return EXIT_FAILURE;
    }

    SDL_Window* win = SDL_CreateWindow("Hello World!", -1, -1, 620, 387, SDL_WINDOW_SHOWN);
    if (win == NULL) {
        fprintf(stderr, "SDL_CreateWindow Error: %s\n", SDL_GetError());
        return EXIT_FAILURE;
    }

    SDL_Renderer* ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (ren == NULL) {
        fprintf(stderr, "SDL_CreateRenderer Error: %s\n", SDL_GetError());
        SDL_DestroyWindow(win);
        SDL_Quit();
        return EXIT_FAILURE;
    }

    SDL_Surface* bmp = SDL_LoadBMP("../img/grumpy-cat.bmp");
    if (bmp == NULL) {
        fprintf(stderr, "SDL_LoadBMP Error: %s\n", SDL_GetError());
        SDL_DestroyRenderer(ren);
        SDL_DestroyWindow(win);
        SDL_Quit();
        return EXIT_FAILURE;
    }

    SDL_Texture* tex = SDL_CreateTextureFromSurface(ren, bmp);
    if (tex == NULL) {
        fprintf(stderr, "SDL_CreateTextureFromSurface Error: %s\n", SDL_GetError());
        SDL_FreeSurface(bmp);
        SDL_DestroyRenderer(ren);
        SDL_DestroyWindow(win);
        SDL_Quit();
        return EXIT_FAILURE;
    }
    SDL_FreeSurface(bmp);

    SDL_Event e;
    bool quit = false;
    Uint32 startTime = SDL_GetTicks();

    while (!quit) {
        while (SDL_PollEvent(&e)) {
            if (e.type == SDL_QUIT) {
                quit = true;
            }
            if (e.type == SDL_KEYDOWN) {
                if (e.key.keysym.sym == SDLK_ESCAPE) {
                    quit = true;
                }
            }
        }

        Uint32 elapsedTime = SDL_GetTicks() - startTime;
        if (elapsedTime > 2000) {
            break;
        }

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
