require "../sdl"

SDL.init(SDL::Init::VIDEO)
at_exit { SDL.quit }

window = SDL::Window.new("SDL tutorial", 640, 480)
bmp = SDL.load_bmp(File.join(__DIR__, "data", "hello_world.bmp"))

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

  bmp.blit(window.surface)
  window.update
end
