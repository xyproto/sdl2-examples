require "sdl"
require "sdl/image"

begin
 SDL.init(SDL::Init::VIDEO)
 at_exit { SDL.quit }

 SDL::IMG.init(SDL::IMG::Init::PNG)
 at_exit { SDL::IMG.quit }

 window = SDL::Window.new("Hello World!", 620, 387)
 renderer = SDL::Renderer.new(window, SDL::Renderer::Flags::ACCELERATED | SDL::Renderer::Flags::PRESENTVSYNC)

 png = SDL::IMG.load(File.join("..", "img", "grumpy-cat.png"), renderer)
 texture = png

 quit = false
 start_time = Time.monotonic.total_milliseconds.to_i

 while !quit
   case event = SDL::Event.poll
   when SDL::Event::Quit
     quit = true
    when SDL::Event::Keyboard
      quit = true if event.keydown? && event.sym.escape?
   end

   elapsed_time = Time.monotonic.total_milliseconds.to_i - start_time
   break if elapsed_time > 2000

   renderer.clear
   renderer.copy(texture)
   renderer.present
   sleep(100.milliseconds)
 end

rescue ex
 abort "Error: #{ex.message}"
end
