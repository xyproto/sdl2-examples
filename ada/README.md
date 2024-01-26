# Ada

## Requirements

### macOS

* Install these packages from ie. Homebrew: `make sdl2 sdl2_image sdl2_mixer sdl2_ttf`.
* Install Alire / `alr`.
** For macOS on Intel, install it from the [alire-project](https://github.com/alire-project/alire/releases). The latest beta or nightly release might be needed.
** For macOS on Apple silicon, install the `aarch64` toolchain (`alr` and the compiler) from [github.com/simonjwright](https://github.com/simonjwright/alire-index.mac).

### Arch Linux

* Install `sdl2` and `sdl2_image`.
* Also install `alire`, `gprbuild` and `sdlada` from AUR (`sdlada` provides `/usr/share/gpr/manifests/sdlada`).
* `alire` and `gprbuild` may currently not be straightforward to install from AUR.

## Building

* Run `make` or `alr update; alr build`.

---

Pull requests are welcome.
