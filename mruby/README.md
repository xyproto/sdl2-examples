# Ruby

This example uses MRuby.

* Install the SDL2 libraries and include files.

* Install [MRuby](https://github.com/mruby/mruby) and [mruby-sdl2](https://github.com/crimsonwoods/mruby-sdl2).

The installation of `mruby-sdl2` may be tricky, and may involve recompiling mruby and also adding this line to `build_config.rb`:

  conf.gem :github => 'crimsonwoods/mruby-sdl2', :branch => 'master'

Consult the [mruby-sdl2 project](https://github.com/crimsonwoods/mruby-sdl2) for more information.

* Run the SDL2 sample with `mruby main.rb`.
