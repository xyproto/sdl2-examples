require "../sdl"

SDL.init(SDL::Init::VIDEO)
at_exit { SDL.quit }

window = SDL::Window.new("SDL tutorial", 640, 480)

loop do
  event = SDL::Event.wait

  case event
  when SDL::Event::Quit
    break
  when SDL::Event::Keyboard
    if event.mod.lctrl? && event.sym.q?
      break
    end
  end

  window.surface.fill(255, 255, 255)
  window.update
end
