require "../src/sdl"
require "../src/image"

SDL.init(SDL::Init::VIDEO)
at_exit { SDL.quit }

window = SDL::Window.new("SDL Tutorial", 640, 480)
renderer = SDL::Renderer.new(window)

SDL::IMG.init(SDL::IMG::Init::PNG)
texture = SDL::IMG.load(File.join(__DIR__, "data", "loaded.png"), renderer)

loop do
  case event = SDL::Event.wait
  when SDL::Event::Quit
    break
  end

  renderer.draw_color = SDL::Color[255, 0, 0, 255]
  renderer.clear

  renderer.copy(texture, dstrect: SDL::Rect[20, 20, 600, 440])

  renderer.present
end
