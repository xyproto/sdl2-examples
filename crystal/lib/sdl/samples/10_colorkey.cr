require "../src/sdl"
require "../src/image"

SDL.init(SDL::Init::VIDEO)
at_exit { SDL.quit }

SDL::IMG.init(SDL::IMG::Init::PNG)
at_exit { SDL::IMG.quit }

SDL.set_hint(SDL::Hint::RENDER_SCALE_QUALITY, "1")
window = SDL::Window.new("SDL tutorial", 640, 480)
renderer = SDL::Renderer.new window

background = SDL::IMG.load(File.join(__DIR__, "data", "background.png"), renderer)

# image has a colored background, declare the color as transparent:
image = SDL::IMG.load(File.join(__DIR__, "data", "foo.png"))
image.color_key = {0, 255, 255}
foo = SDL::Texture.from(image, renderer)

# image has a transparent background color (SDL_image sets the colorkey):
#foo = IMG.load(File.join(__DIR__, "data", "foo2.png"), renderer)

loop do
  case event = SDL::Event.poll
  when SDL::Event::Quit
    break
  end

  # clear sreen in white
  renderer.draw_color = SDL::Color[255, 255, 255, 255]
  renderer.clear

  renderer.copy(background)
  renderer.copy(foo, dstrect: SDL::Rect[240, 190, foo.width, foo.height])

  renderer.present
end
