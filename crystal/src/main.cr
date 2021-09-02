require "sdl"
require "sdl/image"

begin
  SDL.init(SDL::Init::VIDEO)

  at_exit { SDL.quit }

  window = SDL::Window.new("Hello, World!", 620, 387)

  SDL::IMG.init(SDL::IMG::Init::PNG)
  at_exit { SDL::IMG.quit }

  png = SDL::IMG.load(File.join("..", "img", "grumpy-cat.png"))
  png = png.convert(window.surface)

  start_time = Time.utc
  loop do
    png.blit(window.surface)
    window.update

    if (Time.utc - start_time).seconds > 2
      break
    end
  end

rescue ex
  puts "Error: " + ex.message.to_s
  exit 1
end
