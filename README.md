SDL2 Examples
=============

[![Build Status](https://travis-ci.com/xyproto/sdl2-examples.svg?branch=master)](https://travis-ci.com/xyproto/sdl2-examples)

"hello world" for SDL2 for various programming languages.

Each sample creates a window, displays an image, then waits two seconds and quits.


Requirements
------------

* The SDL 2 library.
* See the README or README.md file per sample for more information.


Requirements for some of the languages
--------------------------------------

* C compiler that supports C89 (ANSI C), C99 or C11, for the C samples
* A C++ compiler for the C++ sample
* GCC 4.8 or later (or clang++) for the C++11 sample
* Go 1.1 or later and the sdl2 go package (`go get github.com/veandco/go-sdl2/sdl`)
* MRuby with SDL2 added to the configuration file
* Nimrod 0.9.4 and sdl2 installed with babel
* Python 2 or 3 and PySDL2
* FPC 2.6.4 (or later than 2.4.0, must have Uint8, Uint16 and Uint32)
* Lua (tested with Lua 5.2) and lua-sdl2


Contribute
-----------

The following samples are missing, but wanted:

* OCaml
* Zig
* Modern C++ in combination with Bazel

The following samples needs and upgrade and/or being looked at:

* Rust (should be upgraded to use version `0.33` or later of the `sdl2` crate)

Pull requests are welcome.


General information
----------------------

* License: MIT
