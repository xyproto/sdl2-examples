## Requirements

* If you can use Alire ([here](https://ada-lang.io) and [here](https://alire.ada.dev/docs/)), then it should meet all the requirements (but see macOS notes below).
* If not,
  - Ada compiler
  - SDLAda binding to SDL2 and SDL2\_image
  - GPRbuild

## Build with Alire ##

Once `alr` is installed, just `alr build`. This will, as necessary, download a compiler, the build tool gprbuild, SDLada, and external libraries (SDL2 and SDL2\_image; also, but not used in this example, SDL\_mixer and SDL\_ttf), and then proceed to build the example (in `bin/main`).

### macOS ###

This suite (and SDLada) aren't written to use the framework SDL2 libraries.  A sufficiently recent `alr` will install external libraries using either [Homebrew](https://brew.sh) or [MacPorts](https://www.macports.org).

### macOS on Intel silicon ###

To get a "sufficiently recent" `alr`, visit the [v2.0.0-beta1](https://github.com/alire-project/alire/releases/tag/v2.0.0-beta1) or [nightly](https://github.com/alire-project/alire/releases/tag/nightly) releases.

### macOS on Apple silicon ###

Because the package managers will install `aarch64` (`arm64`) binaries when run on Apple silicon, you'll need to use an `aarch64` toolchain (`alr` and the compiler). These aren't (yet) available from the official Alire repositories, but they are available [here](https://github.com/simonjwright/alire-index.mac).

## Build on Arch Linux

* `/usr/share/gpr/manifests/sdlada` must exist (comes with the `sdlada` package in AUR).
* `sdl2` and `sdl2_image` must be installed.

Then just:

    gprbuild -largs $(pkg-config sdl2 SDL_image --libs)

----

Pull requests are welcome.
