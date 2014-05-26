SDL2::init

X = SDL2::Video::Window::SDL_WINDOWPOS_UNDEFINED
Y = SDL2::Video::Window::SDL_WINDOWPOS_UNDEFINED
W = 960
H = 540
FLAGS = SDL2::Video::Window::SDL_WINDOW_SHOWN

begin
  SDL2::Video::init
  begin
    win = SDL2::Video::Window.new "sample", X, Y, W, H, FLAGS
    ren = SDL2::Video::Renderer.new(win)
    bmp = SDL2::Video::Surface::load_bmp("../img/boxes.bmp")
    tex = SDL2::Video::Texture.new(ren, bmp)

    # Show the image 
    ren.clear
    ren.copy(tex)
    ren.present

    # Wait 2 seconds
    SDL2::delay(2000)

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
