## Requirements

- Ada compiler
- SDLAda binding to SDL2, SDL2_image, and SDL2_ttf
- GPRbuild

## Build on Arch Linux

* `/usr/share/gpr/manifests/sdlada` must exist (comes with the `sdlada` package in AUR).
* `sdl2` and `sdl2_image` must be installed.

Then just:

    gprbuild -largs $(pkg-config sdl2 SDL_image --libs)

## Build on macOS

* Change `sdlada` to `SDLAda` in `main.gpr`.
* SDL2 and SDL2_image must be installed in `/Library/Frameworks`.

Then just:

    gprbuild -largs -F/Library/Frameworks -framework SDL2 -framework SDL2_image
