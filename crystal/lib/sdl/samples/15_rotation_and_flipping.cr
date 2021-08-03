require "../src/sdl"
require "../src/image"

SDL.init(SDL::Init::VIDEO); at_exit { SDL.quit }
SDL::IMG.init(SDL::IMG::Init::PNG); at_exit { SDL::IMG.quit }

window = SDL::Window.new("SDL tutorial", 640, 480)
renderer = SDL::Renderer.new(window, SDL::Renderer::Flags::ACCELERATED | SDL::Renderer::Flags::PRESENTVSYNC)

arrow = SDL::IMG.load(File.join(__DIR__, "data", "arrow.png"), renderer)
degrees = 0
flip = SDL::Renderer::Flip::NONE

loop do
  case event = SDL::Event.wait
  when SDL::Event::Quit
    break
  when SDL::Event::Keyboard
    case event.sym
    when .a? then degrees -= 60
    when .d? then degrees += 60
    when .q? then flip = SDL::Renderer::Flip::HORIZONTAL
    when .w? then flip = SDL::Renderer::Flip::NONE
    when .e? then flip = SDL::Renderer::Flip::VERTICAL
    end if event.keydown?
  end

  renderer.draw_color = SDL::Color[255, 255, 255, 255]
  renderer.clear

  x = (window.width - arrow.width) / 2
  y = (window.height - arrow.height) / 2
  renderer.copy(arrow, dstrect: SDL::Rect[x, y, arrow.width, arrow.height], angle: degrees, flip: flip)

  renderer.present
end
