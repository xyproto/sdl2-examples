require "./sdl.cr/src/sdl"
require "./sdl.cr/src/image"

SDL.init(SDL::Init::VIDEO)
at_exit { SDL.quit }
window = SDL::Window.new("Hello, World!", 960, 540)

SDL::IMG.init(SDL::IMG::Init::PNG)
at_exit { SDL::IMG.quit }

png = SDL::IMG.load(File.join(__DIR__, "..", "img", "boxes.png"))
png = png.convert(window.surface)

start_time = Time.now
loop do
  png.blit(window.surface)
  window.update

  if (Time.now - start_time).seconds > 2
    break
  end
end
