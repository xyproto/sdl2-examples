require "../src/sdl"
require "../src/ttf"

SDL.init(SDL::Init::VIDEO); at_exit { SDL.quit }
SDL::TTF.init; at_exit { SDL::TTF.quit }

window = SDL::Window.new("SDL tutorial", 640, 480)
renderer = SDL::Renderer.new(window, SDL::Renderer::Flags::ACCELERATED | SDL::Renderer::Flags::PRESENTVSYNC)

font = SDL::TTF::Font.new(File.join(__DIR__, "data", "lazy.ttf"), 28)

loop do
  case event = SDL::Event.wait
  when SDL::Event::Quit
    break
  end

  renderer.draw_color = SDL::Color[255, 255, 255, 255]
  renderer.clear

  color = SDL::Color[255, 0, 0, 255]
  surface = font.render_shaded("The quick brow fox jumps over the lazy dog.", color, renderer.draw_color)

  x = (window.width - surface.width) / 2
  y = (window.height - surface.height) / 2
  renderer.copy(surface, dstrect: SDL::Rect[x, y, surface.width, surface.height])

  renderer.present
end
