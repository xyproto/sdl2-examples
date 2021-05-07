use "lib:SDL2"
use "time"

use @SDL_Init[I32](flags: U32)
use @SDL_CreateWindow[Pointer[_SDLWindow]](title: Pointer[U8] tag, x: I32, y: I32, w: I32, h: I32, flags: U32)
use @SDL_CreateRenderer[Pointer[_SDLRenderer]](window: Pointer[_SDLWindow], index: I32, flags: U32)
use @SDL_CreateTextureFromSurface[Pointer[_SDLTexture]](renderer: Pointer[_SDLRenderer], surface: Pointer[_SDLSurface])
use @SDL_DestroyRenderer[None](renderer: Pointer[_SDLRenderer])
use @SDL_DestroyWindow[None](window: Pointer[_SDLWindow])
use @SDL_FreeSurface[None](surface: Pointer[_SDLSurface])
use @SDL_DestroyTexture[None](texture: Pointer[_SDLTexture])
use @SDL_RenderClear[I32](renderer: Pointer[_SDLRenderer])
use @SDL_RenderCopy[I32](renderer: Pointer[_SDLRenderer], texture: Pointer[_SDLTexture], srcrect: Pointer[_SDLRect], dstrect: Pointer[_SDLRect])
use @SDL_RenderPresent[None](renderer: Pointer[_SDLRenderer])
use @SDL_LoadBMP_RW[Pointer[_SDLSurface]](src: Pointer[_SDLRWops], freesrc: I32)
use @SDL_RWFromFile[Pointer[_SDLRWops]](filename: Pointer[U8] tag, permissions: Pointer[U8] tag)
use @SDL_Quit[None]()

struct _SDLRect
  var x: I32 = 0
  var y: I32 = 0
  var w: I32 = 0
  var h: I32 = 0

  new create(x1: I32, y1: I32, w1: I32, h1: I32) =>
    x = x1
    y = y1
    w = w1
    h = h1

primitive _SDLWindow
primitive _SDLRenderer
primitive _SDLSurface
primitive _SDLTexture
primitive _SDLRWops

primitive SDL2
  fun init_video(): U32 => 0x00000020
  fun window_shown(): U32 => 0x00000004
  fun renderer_accelerated(): U32 => 0x00000002
  fun renderer_presentvsync(): U32 => 0x00000004

actor Game
  let win: Pointer[_SDLWindow]
  let ren: Pointer[_SDLRenderer]
  let bmp: Pointer[_SDLSurface]
  let tex: Pointer[_SDLTexture]
  let rwop: Pointer[_SDLRWops]
  let timers: Timers = Timers
  let render_loop: Timer tag
  let windowpos_undefined: I32 = 0x1FFF0000

  new create() =>
    win = @SDL_CreateWindow("Hello World!".cstring(), windowpos_undefined, windowpos_undefined, 620, 387, SDL2.window_shown())
    ren = @SDL_CreateRenderer(win, -1, SDL2.renderer_accelerated() or SDL2.renderer_presentvsync())
    rwop = @SDL_RWFromFile("../img/grumpy-cat.bmp".cstring(), "rb".cstring())
    bmp = @SDL_LoadBMP_RW(rwop, 1)
    tex = @SDL_CreateTextureFromSurface(ren, bmp)

    let quitter = Timer(object iso
                          let _game:Game = this
                          fun ref apply(timer:Timer, count:U64):Bool =>
                            _game.quit()
                            false
                        fun ref cancel(timer:Timer) => None
                      end, 1_000_000_000 * 2, 0) // 2 Second timeout
    timers(consume quitter)

    let timer = Timer(object iso
                        let _game:Game = this
                        fun ref apply(timer:Timer, count:U64):Bool =>
                          _game.loop()
                          true
                        fun ref cancel(timer:Timer) => None
                      end, 0, 100_000_000) // 100ms timeout
    render_loop = timer
    timers(consume timer)

  be loop() =>
   @SDL_RenderClear(ren)
   @SDL_RenderCopy(ren, tex, Pointer[_SDLRect], Pointer[_SDLRect])
   @SDL_RenderPresent(ren)

  be quit() =>
    dispose()

  be dispose() =>
    timers.cancel(render_loop)
    @SDL_FreeSurface(bmp)
    @SDL_DestroyTexture(tex)
    @SDL_DestroyRenderer(ren)
    @SDL_DestroyWindow(win)
    @SDL_Quit()

actor Main
  new create(env:Env) =>
    @SDL_Init(SDL2.init_video())
    let game = Game
