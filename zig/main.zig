const sdl2 = @cImport(@cInclude("SDL2/SDL.h"));
const print = @import("std").debug.print;

pub fn main() !u8 {
    if (sdl2.SDL_Init(sdl2.SDL_INIT_EVERYTHING) != 0) {
        print("SDL_Init Error: {}\n", .{sdl2.SDL_GetError()});
        return 1;
    }

    var win: ?*sdl2.SDL_Window = sdl2.SDL_CreateWindow("Hello World!", 100, 100, 620, 387, sdl2.SDL_WINDOW_SHOWN);
    if (win == null) {
        print("SDL_CreateWindow Error: {}\n", .{sdl2.SDL_GetError()});
        return 1;
    }

    var ren: ?*sdl2.SDL_Renderer = sdl2.SDL_CreateRenderer(win, -1, sdl2.SDL_RENDERER_ACCELERATED | sdl2.SDL_RENDERER_PRESENTVSYNC);
    if (ren == null) {
        print("SDL_CreateRenderer Error: {}\n", .{sdl2.SDL_GetError()});
        if (win != null) {
            sdl2.SDL_DestroyWindow(win);
        }
        sdl2.SDL_Quit();
        return 1;
    }

    var bmp: ?*sdl2.SDL_Surface = sdl2.SDL_LoadBMP("../img/grumpy-cat.bmp");
    if (bmp == null) {
        print("SDL_LoadBMP Error: {}\n", .{sdl2.SDL_GetError()});
        if (ren != null) {
            sdl2.SDL_DestroyRenderer(ren);
        }
        if (win != null) {
            sdl2.SDL_DestroyWindow(win);
        }
        sdl2.SDL_Quit();
        return 1;
    }

    var tex: ?*sdl2.SDL_Texture = sdl2.SDL_CreateTextureFromSurface(ren, bmp);
    if (tex == null) {
        print("SDL_CreateTextureFromSurface Error: {}\n", .{sdl2.SDL_GetError()});
        if (bmp != null) {
            sdl2.SDL_FreeSurface(bmp);
        }
        if (ren != null) {
            sdl2.SDL_DestroyRenderer(ren);
        }
        if (win != null) {
            sdl2.SDL_DestroyWindow(win);
        }
        sdl2.SDL_Quit();
        return 1;
    }
    sdl2.SDL_FreeSurface(bmp);

    var i: usize = 0;
    while (i < 20) {
        _ = sdl2.SDL_RenderClear(ren);
        _ = sdl2.SDL_RenderCopy(ren, tex, null, null);
        sdl2.SDL_RenderPresent(ren);
        sdl2.SDL_Delay(100);
        i += 1;
    }

    sdl2.SDL_DestroyTexture(tex);
    sdl2.SDL_DestroyRenderer(ren);
    sdl2.SDL_DestroyWindow(win);
    sdl2.SDL_Quit();

    return 0;
}
