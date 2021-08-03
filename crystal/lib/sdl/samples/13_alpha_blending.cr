require "../src/sdl"
require "../src/image"

SDL.init(SDL::Init::VIDEO); at_exit { SDL.quit }
SDL::IMG.init(SDL::IMG::Init::PNG); at_exit { SDL::IMG.quit }

window = SDL::Window.new("SDL tutorial", 640, 480)
renderer = SDL::Renderer.new(window)

background = SDL::IMG.load(File.join(__DIR__, "data", "fadein.png"), renderer)
modulated = SDL::IMG.load(File.join(__DIR__, "data", "fadeout.png"), renderer)
a = 255

loop do
  case event = SDL::Event.wait
  when SDL::Event::Quit
    break
  when SDL::Event::Keyboard
    case event.sym
    when .w? then a += 32
    when .s? then a -= 32
    end if event.keydown?
  end

  a = a.clamp(0, 255)

  renderer.draw_color = SDL::Color[255, 255, 255, 255]
  renderer.clear

  renderer.copy(background)

  modulated.alpha_mod = a
  renderer.copy(modulated)

  renderer.present
end
