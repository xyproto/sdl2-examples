import sdl2

var 
  win: PWindow
  ren: PRenderer
  bmp: PSurface
  tex: PTexture

discard Init(INIT_EVERYTHING)

win = CreateWindow("Hello World!", 100, 100, 960, 540, SDL_WINDOW_SHOWN)
if win == nil:
  echo("CreateWindow Error: ", GetError())
  quit(1)

ren = CreateRenderer(win, -1, Renderer_Accelerated or Renderer_PresentVsync)
if ren == nil:
  echo("CreateRenderer Error: ", GetError())
  quit(1)

bmp = LoadBMP("../img/boxes.bmp")
if bmp == nil:
  echo("LoadBMP Error: ", GetError())
  quit(1)

tex = CreateTextureFromSurface(ren, bmp)
if tex == nil:
  echo("CreateTextureFromSurface Error: ", GetError())
  quit(1)
FreeSurface(bmp)

ren.Clear
Copy(ren, tex, nil, nil)
ren.Present

Delay(2000)

destroy tex
destroy ren
destroy win

sdl2.Quit()
