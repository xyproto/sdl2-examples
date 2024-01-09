import kotlin.OptIn
import kotlinx.cinterop.*
import sdl2.*

@OptIn(ExperimentalForeignApi::class)
fun main() {
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        println("SDL_Init failed: ${SDL_GetError()?.toKString()}")
        return
    }

    val window = SDL_CreateWindow(
        "Hello World!", // Match the window title with the C program
        100, 100,
        620, 387,
        SDL_WINDOW_SHOWN
    )
    if (window == null) {
        println("SDL_CreateWindow failed: ${SDL_GetError()?.toKString()}")
        SDL_Quit()
        return
    }

    val renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC)
    if (renderer == null) {
        println("SDL_CreateRenderer failed: ${SDL_GetError()?.toKString()}")
        SDL_DestroyWindow(window)
        SDL_Quit()
        return
    }

    val imageFilename = "../img/grumpy-cat.bmp"
    val rwop = SDL_RWFromFile(imageFilename, "rb")
    if (rwop == null) {
        println("SDL_RWFromFile failed: ${SDL_GetError()?.toKString()}")
        SDL_DestroyRenderer(renderer)
        SDL_DestroyWindow(window)
        SDL_Quit()
        return
    }

    val bmp = SDL_LoadBMP_RW(rwop, 1)
    if (bmp == null) {
        println("SDL_LoadBMP_RW failed: ${SDL_GetError()?.toKString()}")
        SDL_DestroyRenderer(renderer)
        SDL_DestroyWindow(window)
        SDL_Quit()
        return
    }

    val tex = SDL_CreateTextureFromSurface(renderer, bmp)
    if (tex == null) {
        println("SDL_CreateTextureFromSurface failed: ${SDL_GetError()?.toKString()}")
        SDL_FreeSurface(bmp)
        SDL_DestroyRenderer(renderer)
        SDL_DestroyWindow(window)
        SDL_Quit()
        return
    }

    SDL_FreeSurface(bmp)

    repeat(20) {
        SDL_RenderClear(renderer)
        SDL_RenderCopy(renderer, tex, null, null)
        SDL_RenderPresent(renderer)
        SDL_Delay(100u)
    }

    SDL_DestroyTexture(tex)
    SDL_DestroyRenderer(renderer)
    SDL_DestroyWindow(window)
    SDL_Quit()
}
