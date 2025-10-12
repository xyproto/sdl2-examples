require "sdl"
require "sdl/image"

begin
  SDL.init(SDL::Init::VIDEO)
  at_exit { SDL.quit }

  SDL::IMG.init(SDL::IMG::Init::PNG)
  at_exit { SDL::IMG.quit }

  window = SDL::Window.new("Hello World!", 620, 387)
  renderer = SDL::Renderer.new(window, SDL::Renderer::Flags::ACCELERATED | SDL::Renderer::Flags::PRESENTVSYNC)

  texture = SDL::IMG.load(File.join(__DIR__, "..", "img", "grumpy-cat.png"), renderer)

  start_time = Time.monotonic
  quit = false

  until quit
    case event = SDL::Event.poll
    when SDL::Event::Quit
      quit = true
    when SDL::Event::Keyboard
      quit = true if event.keydown? && event.sym.escape?
    end

    break if (Time.monotonic - start_time) > 2.seconds

    renderer.clear
    renderer.copy(texture)
    renderer.present

    sleep(100.milliseconds)
  end
rescue ex
  abort "Error: #{ex.message}"
end
