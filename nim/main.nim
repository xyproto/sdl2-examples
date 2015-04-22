import sdl2

var 
  win: WindowPtr
  ren: RendererPtr
  bmp: SurfacePtr
  tex: TexturePtr

discard init(INIT_EVERYTHING)

win = createWindow("Hello World!", 100, 100, 960, 540, SDL_WINDOW_SHOWN)
if win == nil:
  echo("createWindow Error: ", getError())
  quit(1)

ren = createRenderer(win, -1, Renderer_Accelerated or Renderer_PresentVsync)
if ren == nil:
  echo("createRenderer Error: ", getError())
  quit(1)

bmp = loadBMP("../img/boxes.bmp")
if bmp == nil:
  echo("loadBMP Error: ", getError())
  quit(1)

tex = createTextureFromSurface(ren, bmp)
if tex == nil:
  echo("createTextureFromSurface Error: ", getError())
  quit(1)
freeSurface(bmp)

ren.clear
copy(ren, tex, nil, nil)
ren.present

delay(2000)

destroy tex
destroy ren
destroy win

sdl2.quit()
