require "../src/sdl"
require "../src/image"

SDL.init(SDL::Init::VIDEO); at_exit { SDL.quit }
SDL::IMG.init(SDL::IMG::Init::PNG); at_exit { SDL::IMG.quit }

window = SDL::Window.new("SDL tutorial", 640, 480)
renderer = SDL::Renderer.new(window)

image = SDL::IMG.load(File.join(__DIR__, "data", "full.png"), renderer)
r = g = b = 255

loop do
  case event = SDL::Event.wait
  when SDL::Event::Quit
    break
  when SDL::Event::Keyboard
    case event.sym
    when .a?
      r += 32
    when .z?
      g += 32
    when .e?
      b += 32
    when .q?
      r -= 32
    when .s?
      g -= 32
    when .d?
      b -= 32
    end if event.keyup?
  end

  r = r.clamp(0, 255)
  g = g.clamp(0, 255)
  b = b.clamp(0, 255)

  renderer.draw_color = SDL::Color[255, 255, 255, 255]
  renderer.clear

  image.color_mod = {r, g, b}
  renderer.copy(image)

  renderer.present
end
