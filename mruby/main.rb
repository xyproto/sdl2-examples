SDL2::init

X = SDL2::Video::Window::SDL_WINDOWPOS_UNDEFINED
Y = SDL2::Video::Window::SDL_WINDOWPOS_UNDEFINED
W = 620
H = 387
FLAGS = SDL2::Video::Window::SDL_WINDOW_SHOWN

# Note that error checking is built in for many of the functions below.

begin
  begin
    win = SDL2::Video::Window.new "Hello World!", X, Y, W, H, FLAGS
    ren = SDL2::Video::Renderer.new(win)
    bmp = SDL2::Video::Surface::load_bmp("../img/grumpy-cat.bmp")
    tex = SDL2::Video::Texture.new(ren, bmp)
    bmp.free

    for i in 0..20
      # Show the image
      ren.clear
      ren.copy(tex)
      ren.present

      # Wait 100 ms
      SDL2::delay(100)
    end

    # Clean up
    tex.destroy
    ren.destroy
    win.destroy
  ensure
    SDL2::Video::quit
  end
ensure
  SDL2::quit
end
