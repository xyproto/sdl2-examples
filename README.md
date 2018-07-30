Hello SDL2!
===========

[![Build Status](https://travis-ci.org/xyproto/sdl2-examples.svg?branch=master)](https://travis-ci.org/xyproto/sdl2-examples)

"hello world" for SDL2 for various programming languages.

Each sample creates a window, displays an image, then waits two seconds and quits.


Requirements
------------

* The SDL 2 library.
* See the README or README.md file per sample for more information.


Requirements for some of the languages
--------------------------------------

* C compiler that supports C11, for the C sample
* A C++ compiler for the C++ sample
* GCC 4.8 or later (or clang++) for the C++11 sample
* Go 1.1 or later and the sdl2 go package (`go get github.com/veandco/go-sdl2/sdl`)
* MRuby with SDL2 added to the configuration file
* Nimrod 0.9.4 and sdl2 installed with babel
* Python 2 or 3 and PySDL2
* FPC 2.6.4 (or later than 2.4.0, must have Uint8, Uint16 and Uint32)
* OCaml and OCamlSDL2
* Lua (tested with Lua 5.2) and lua-sdl2


Samples that does not build
---------------------------

* See the samples that are commented out at the top of the `Makefile` for a list of samples that currently does not build on 64-bit Arch Linux.

Pull requests are welcome!


General information
----------------------

* Author: Alexander F. Rødseth
* License: MIT
