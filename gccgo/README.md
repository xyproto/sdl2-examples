Works with gccgo 8.2.1 and the latest version of go-sdl2.

Requirements:

* The SDL2 library and headers installed on your system.
* For macOS, remember to install the Development Libraries (place the SDL2 framework in `/Library/Frameworks`).
* The latest version of `gccgo` (tested with `gccgo 8.2.1`).
* The `sdl2` package:

    go get github.com/veandco/go-sdl2/sdl

Building:

    make

Running:

    ./main
