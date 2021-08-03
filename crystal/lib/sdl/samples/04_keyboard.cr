require "../sdl"

SDL.init(SDL::Init::VIDEO)
at_exit { SDL.quit }

window = SDL::Window.new("SDL tutorial", 640, 480)

surfaces = {
  default: SDL.load_bmp(File.join(__DIR__, "data", "press.bmp")),
  up: SDL.load_bmp(File.join(__DIR__, "data", "up.bmp")),
  down: SDL.load_bmp(File.join(__DIR__, "data", "down.bmp")),
  left: SDL.load_bmp(File.join(__DIR__, "data", "left.bmp")),
  right: SDL.load_bmp(File.join(__DIR__, "data", "right.bmp")),
}

bmp = surfaces[:default]

loop do
  case event = SDL::Event.wait
  when SDL::Event::Quit
    break
  when SDL::Event::Keyboard
    case event.sym
    when .up?
      bmp = surfaces[:up]
    when .down?
      bmp = surfaces[:down]
    when .left?
      bmp = surfaces[:left]
    when .right?
      bmp = surfaces[:right]
    when .q?
      break
    else
      bmp = surfaces[:default]
    end if event.keyup?
  end

  bmp.blit(window.surface)
  window.update
end
