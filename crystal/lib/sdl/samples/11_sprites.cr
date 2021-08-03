require "../src/sdl"
require "../src/image"

SDL.init(SDL::Init::VIDEO); at_exit { SDL.quit }
SDL::IMG.init(SDL::IMG::Init::PNG); at_exit { SDL::IMG.quit }

window = SDL::Window.new("SDL tutorial", 640, 480)
renderer = SDL::Renderer.new(window)

image = SDL::IMG.load(File.join(__DIR__, "data", "sprites.png"))
image.color_key = {0, 255, 255}
sprite = SDL::Texture.from(image, renderer)

width, height = renderer.output_size

loop do
  case event = SDL::Event.wait
  when SDL::Event::Quit
    break
  end

  renderer.draw_color = SDL::Color[255, 255, 255, 255]
  renderer.clear

  renderer.copy(sprite, SDL::Rect[0, 0, 100, 100], SDL::Rect[0, 0, 100, 100])
  renderer.copy(sprite, SDL::Rect[100, 0, 100, 100], SDL::Rect[width - 100, 0, 100, 100])
  renderer.copy(sprite, SDL::Rect[0, 100, 100, 100], SDL::Rect[0, height - 100, 100, 100])
  renderer.copy(sprite, SDL::Rect[100, 100, 100, 100], SDL::Rect[width - 100, height - 100, 100, 100])

  renderer.present
end
