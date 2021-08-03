require "../src/sdl"
require "../src/image"

SDL.init(SDL::Init::VIDEO)
at_exit { SDL.quit }

window = SDL::Window.new("SDL tutorial", 640, 480)
renderer = SDL::Renderer.new(window)

width, height = window.size
png = SDL::IMG.load(File.join(__DIR__, "data", "loaded.png"), renderer)

loop do
  case event = SDL::Event.wait
  when SDL::Event::Quit
    break
  end

  # clear sreen in white
  renderer.draw_color = SDL::Color[255, 255, 0, 255]
  renderer.clear

  # top left
  renderer.viewport = SDL::Rect[20, 20, 290, 210]
  renderer.copy(png)

  # top left
  renderer.viewport = SDL::Rect[330, 20, 290, 210]
  renderer.copy(png)

  # bottom
  renderer.viewport = SDL::Rect[20, 250, 600, 210]
  renderer.copy(png)

  renderer.present
end
