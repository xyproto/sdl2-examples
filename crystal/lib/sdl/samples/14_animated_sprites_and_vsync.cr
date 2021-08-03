require "../src/sdl"
require "../src/image"

SDL.init(SDL::Init::VIDEO); at_exit { SDL.quit }
SDL::IMG.init(SDL::IMG::Init::PNG); at_exit { SDL::IMG.quit }

width, height = 640, 480
window = SDL::Window.new("SDL tutorial", 640, 480)
renderer = SDL::Renderer.new(window, SDL::Renderer::Flags::ACCELERATED | SDL::Renderer::Flags::PRESENTVSYNC)

sprite = SDL::IMG.load(File.join(__DIR__, "data", "foo_sprite.png"), renderer)
sprite_clips = StaticArray(SDL::Rect, 4).new do |i|
  SDL::Rect.new(i * 64, 0, 64, 305)
end

frame = 0
slowdown = 6

loop do
  case event = SDL::Event.poll
  when SDL::Event::Quit
    break
  end

  renderer.draw_color = SDL::Color[255, 255, 255, 255]
  renderer.clear

  current_clip = sprite_clips[frame / slowdown]
  x = (window.width - current_clip.w) / 2
  y = (window.height - current_clip.h) / 2
  renderer.copy(sprite, current_clip, SDL::Rect[x, y, current_clip.w, current_clip.h])

  renderer.present

  frame = (frame + 1) % (sprite_clips.size * slowdown)
end
