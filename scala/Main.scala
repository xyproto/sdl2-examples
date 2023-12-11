/*
 * An SDL2 example in Scala Native
 *
 * Inspired by a Scala Native program by @densh: https://gist.github.com/densh/1885e8b03127fd52ff659505d8b3b76b
 */

import scalanative.unsafe._
import scala.scalanative.unsigned._

import SDL._
import SDLNonExternal._

@extern
@link("SDL2")
object SDL {

  // --- Types used by the external SDL2 functions ---

  type Window = CStruct0
  type Renderer = CStruct0
  type Surface = CStruct0
  type Texture = CStruct0
  type SDL_RWops = CStruct0
  type Rect = CStruct4[CInt, CInt, CInt, CInt]

  // -- External functions that are provided by the SDL2 library ---

  def SDL_Init(flags: CUnsignedInt): CInt = extern

  def SDL_CreateWindow(
      title: CString,
      x: CUnsignedInt,
      y: CUnsignedInt,
      w: Int,
      h: Int,
      flags: CUnsignedInt
  ): Ptr[Window] = extern

  def SDL_CreateRenderer(
      win: Ptr[Window],
      index: CInt,
      flags: CUnsignedInt
  ): Ptr[Renderer] = extern

  def SDL_CreateTextureFromSurface(
      ren: Ptr[Renderer],
      surf: Ptr[Surface]
  ): Ptr[Texture] = extern

  def SDL_DestroyRenderer(ren: Ptr[Renderer]): Unit = extern

  def SDL_DestroyWindow(ren: Ptr[Window]): Unit = extern

  def SDL_FreeSurface(ren: Ptr[Surface]): Unit = extern

  def SDL_DestroyTexture(tex: Ptr[Texture]): Unit = extern

  def SDL_RenderClear(ren: Ptr[Renderer]): Unit = extern

  def SDL_RenderCopy(
      ren: Ptr[Renderer],
      tex: Ptr[Texture],
      src: Ptr[Rect],
      dst: Ptr[Rect]
  ): CInt = extern

  def SDL_RenderPresent(ren: Ptr[Renderer]): Unit = extern

  def SDL_LoadBMP_RW(src: Ptr[SDL_RWops], freesrc: CInt): Ptr[Surface] = extern

  def SDL_RWFromFile(filename: CString, permissions: CString): Ptr[SDL_RWops] =
    extern

  def SDL_Quit(): Unit = extern

  def SDL_GetError(): CString = extern

  def SDL_Delay(ms: CUnsignedInt): Unit = extern
}

object SDLNonExternal {

  // --- SDL2 constants found in the SDL2 headers ---

  // From SDL.h
  final val SDL_INIT_VIDEO = 0x00000020.toUInt

  // From SDL_video.h
  final val SDL_WINDOWPOS_UNDEFINED = 0x1FFF0000.toUInt
  final val SDL_WINDOW_SHOWN = 0x00000004.toUInt

  // From SDL_render.h
  final val SDL_RENDERER_ACCELERATED: UInt = 0x00000002.toUInt
  final val SDL_RENDERER_PRESENTVSYNC: UInt = 0x00000004.toUInt

  // printErr is a helper function that prints a topic for the error,
  // and the error message from SDL_GetError() to stderr.
  def printErr(topic: String): Unit = {
    Console.err.println(s"$topic Error: ${fromCString(SDL.SDL_GetError())}")
  }

}

class HelloSDL {

  val title = c"Hello, World!"
  val width = 620
  val height = 387
  val imageFilename = c"../img/grumpy-cat.bmp"

  var win: Ptr[Window] = _
  var ren: Ptr[Renderer] = _
  var rwop: Ptr[SDL_RWops] = _
  var bmp: Ptr[Surface] = _
  var tex: Ptr[Texture] = _

  def init(): Boolean = {
    if (SDL_Init(SDL_INIT_VIDEO.toUInt) != 0) {
      printErr("SDL_Init")
      return false
    }

    win = SDL_CreateWindow(
      title,
      SDL_WINDOWPOS_UNDEFINED,
      SDL_WINDOWPOS_UNDEFINED,
      width,
      height,
      SDL_WINDOW_SHOWN
    )
    if (win == null) {
      printErr("SDL_CreateWindow")
      return false
    }

    ren = SDL_CreateRenderer(
      win,
      -1,
      SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC
    )
    if (ren == null) {
      printErr("SDL_CreateRenderer")
      SDL_DestroyWindow(win)
      return false
    }

    rwop = SDL_RWFromFile(imageFilename, c"rb")
    if (rwop == null) {
      printErr("SDL_RWFromFile")
      SDL_DestroyRenderer(ren)
      SDL_DestroyWindow(win)
      return false
    }

    bmp = SDL_LoadBMP_RW(rwop, 1) // this also frees rwop
    if (bmp == null) {
      printErr("SDL_LoadBMP_RW")
      SDL_DestroyRenderer(ren)
      SDL_DestroyWindow(win)
      return false
    }

    tex = SDL_CreateTextureFromSurface(ren, bmp)
    if (tex == null) {
      printErr("SDL_CreateTextureFromSurface")
      SDL_FreeSurface(bmp)
      SDL_DestroyRenderer(ren)
      SDL_DestroyWindow(win)
      return false
    }

    SDL_FreeSurface(bmp)

    true
  }

  def loop(): Unit = {
    // Repeat 20 times
    (1 to 20).foreach { _ =>
      SDL_RenderClear(ren)
      SDL_RenderCopy(ren, tex, null, null)
      SDL_RenderPresent(ren)
      SDL_Delay(100.toUInt)
    }
  }

  def destroy(): Unit = {
    SDL_DestroyTexture(tex)
    SDL_DestroyRenderer(ren)
    SDL_DestroyWindow(win)
  }

  def quit(): Unit = {
    SDL_Quit()
  }

  def run(): Unit = {
    if (init()) {
      loop()
      destroy()
    }
    quit()
  }

}

object Main extends App {
  new HelloSDL().run()
}
