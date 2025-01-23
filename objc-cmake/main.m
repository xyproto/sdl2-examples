#import <Foundation/Foundation.h>
#import <SDL2/SDL.h>

@interface SDLWrapper : NSObject
+ (SDL_Window*)createWindowWithTitle:(const char*)title
                                   x:(int)x
                                   y:(int)y
                               width:(int)width
                              height:(int)height
                               flags:(Uint32)flags;
+ (SDL_Renderer*)createRendererForWindow:(SDL_Window*)window
                                  driver:(int)driver
                                   flags:(Uint32)flags;
+ (SDL_Texture*)createTextureFromBMP:(const char*)filename
                            renderer:(SDL_Renderer*)renderer;
+ (void)runEventLoopWithRenderer:(SDL_Renderer*)renderer
                         texture:(SDL_Texture*)texture;
@end

@implementation SDLWrapper

+ (SDL_Window*)createWindowWithTitle:(const char*)title
                                   x:(int)x
                                   y:(int)y
                               width:(int)width
                              height:(int)height
                               flags:(Uint32)flags
{
    return SDL_CreateWindow(title, x, y, width, height, flags);
}

+ (SDL_Renderer*)createRendererForWindow:(SDL_Window*)window
                                  driver:(int)driver
                                   flags:(Uint32)flags
{
    return SDL_CreateRenderer(window, driver, flags);
}

+ (SDL_Texture*)createTextureFromBMP:(const char*)filename
                            renderer:(SDL_Renderer*)renderer
{
    SDL_Surface* surface = SDL_LoadBMP(filename);
    if (!surface) {
        NSLog(@"SDL_LoadBMP Error: %s", SDL_GetError());
        return NULL;
    }

    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surface);
    SDL_FreeSurface(surface);

    if (!texture) {
        NSLog(@"SDL_CreateTextureFromSurface Error: %s", SDL_GetError());
        return NULL;
    }

    return texture;
}

+ (void)runEventLoopWithRenderer:(SDL_Renderer*)renderer
                         texture:(SDL_Texture*)texture
{
    SDL_Event event;
    BOOL quit = NO;
    Uint32 startTime = SDL_GetTicks();

    while (!quit) {
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
            case SDL_QUIT:
                quit = YES;
                break;

            case SDL_KEYDOWN:
                if (event.key.keysym.sym == SDLK_ESCAPE) {
                    quit = YES;
                }
                break;
            }
        }

        Uint32 elapsedTime = SDL_GetTicks() - startTime;
        if (elapsedTime > 2000) {
            break;
        }

        SDL_RenderClear(renderer);
        SDL_RenderCopy(renderer, texture, NULL, NULL);
        SDL_RenderPresent(renderer);
        SDL_Delay(100);
    }
}

@end

int main(int argc, char* argv[])
{
    @autoreleasepool {
        if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
            NSLog(@"SDL_Init Error: %s", SDL_GetError());
            return EXIT_FAILURE;
        }

        SDL_Window* window = [SDLWrapper createWindowWithTitle:"Hello World!"
                                                             x:-1
                                                             y:-1
                                                         width:620
                                                        height:387
                                                         flags:SDL_WINDOW_SHOWN];
        if (!window) {
            NSLog(@"SDL_CreateWindow Error: %s", SDL_GetError());
            SDL_Quit();
            return EXIT_FAILURE;
        }

        SDL_Renderer* renderer = [SDLWrapper createRendererForWindow:window
                                                              driver:-1
                                                               flags:SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC];
        if (!renderer) {
            NSLog(@"SDL_CreateRenderer Error: %s", SDL_GetError());
            SDL_DestroyWindow(window);
            SDL_Quit();
            return EXIT_FAILURE;
        }

        SDL_Texture* texture = [SDLWrapper createTextureFromBMP:"../img/grumpy-cat.bmp"
                                                       renderer:renderer];
        if (!texture) {
            SDL_DestroyRenderer(renderer);
            SDL_DestroyWindow(window);
            SDL_Quit();
            return EXIT_FAILURE;
        }

        [SDLWrapper runEventLoopWithRenderer:renderer texture:texture];

        SDL_DestroyTexture(texture);
        SDL_DestroyRenderer(renderer);
        SDL_DestroyWindow(window);
        SDL_Quit();

        return EXIT_SUCCESS;
    }
}
