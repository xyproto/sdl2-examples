require "../sdl"

def load_bmp(name, window)
  path = File.join(__DIR__, "data", name)
  SDL.load_bmp(path).convert(window.surface)
end

SDL.init(SDL::Init::VIDEO)
at_exit { SDL.quit }

window = SDL::Window.new("SDL tutorial", 640, 480)
bmp = load_bmp("stretch.bmp", window)

loop do
  case event = SDL::Event.wait
  when SDL::Event::Quit
    break
  when SDL::Event::Keyboard
    if event.keyup? && event.sym.q?
      break
    end
  end

  bmp.blit_scaled(window.surface, dstrect: SDL::Rect[20, 20, 600, 440])
  window.update
end
