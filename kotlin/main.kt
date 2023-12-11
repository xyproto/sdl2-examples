import kotlinx.cinterop.*
import sdl.*

@OptIn(ExperimentalForeignApi::class)
fun main() {
  SDL_Init(SDL_INIT_VIDEO)

  val window = SDL_CreateWindow(
      "TEST",
      SDL_WINDOWPOS_CENTERED.toInt(),
      SDL_WINDOWPOS_CENTERED.toInt(),
      640, 480,
      SDL_WINDOW_OPENGL
  )

  if (SDL_Init(SDL_INIT_EVERYTHING) < 0) {
    println("SDL_Init failed: ${SDL_GetError()}");
  }

  val renderer = SDL_CreateRenderer(window, -1, 0u)

  SDL_SetRenderDrawColor(renderer, 255u, 0u, 0u, 255u)

  SDL_RenderClear(renderer)

  SDL_RenderPresent(renderer)

//  memScoped {
//    val event: alloc<SDL_Event>()
//    while (SDL_PollEvent(event.ptr.reinterpret()) != 0) {
//      val eventType = event.type
//      when (eventType) {
//        SDL_QUIT -> return
//      }
//    }
//  }

  SDL_Quit()
}
