local SDL	= require "SDL"
local image	= require "SDL.image"

local ret, err = SDL.init { SDL.flags.Video }
if not ret then
	error(err)
end

local win, err = SDL.createWindow {
	title	= "Hello World!",
	width	= 960,
	height	= 540,
    flags   = SDL.window.Shown
}
if not win then
	error(err)
end

local rdr, err = SDL.createRenderer(win, -1, SDL.rendererFlags.Accelerated + SDL.rendererFlags.PresentVSYNC)
if not rdr then
	error(err)
end

local img, ret = SDL.loadBMP("../img/boxes.bmp")
if not img then
	error(err)
end

tex = rdr:createTextureFromSurface(img)

for i = 1, 20 do
    rdr:clear()
    rdr:copy(tex)
    rdr:present()

    SDL.delay(100)
end
