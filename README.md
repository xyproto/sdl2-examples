Hello SDL2!
===========

[![Build Status](https://travis-ci.org/xyproto/hello_sdl2.svg?branch=master)](https://travis-ci.org/xyproto/hello_sdl2)
<!-- [![Build Status](https://drone.io/github.com/xyproto/browserspeak/status.png)](https://drone.io/github.com/xyproto/browserspeak/latest) -->

"hello world" for SDL2 for various programming languages.

Each sample creates a window, displays an image, then waits two seconds and quits.


Requirements
------------

* The SDL 2 library.
* See the README or README.md file per sample for more information.


Requirements for the various languages
--------------------------------------
* C compiler that supports C99 for the C sample
* A C++ compiler for the C++ sample
* GCC 4.8 or later (or clang++) for the C++11 sample
* Go 1.1 or later and the sdl2 go package (`go get github.com/veandco/go-sdl2/sdl`)
* MRuby with SDL2 added to the configuration file
* Nimrod 0.9.4 and sdl2 installed with babel
* Python 2 or 3 and PySDL2
* FPC 2.6.4 (or later than 2.4.0, must have Uint8, Uint16 and Uint32)
* Rust 0.12.0 or later and rust-sdl2 (from github.com/AngryLawyer/rust-sdl2)
* Rust and Cargo for the rust-cargo example
* OCaml and OCamlSDL2
* Lua (tested with Lua 5.2) and lua-sdl2


Number of times the sample code itself has had to been updated to make it work again
------------------------------------------------------------------------------------

This is usually caused by changes in external dependencies.

* C: 0
* C++: 0
* C++11: 0
* C++11 w/ CMake: 0
* FPC: 1 (new host for the SDL2 git repository)
* GCCGo: 1 (the sdl2 package changed)
* Go: 1 (the sdl2 package changed)
* Lua: 0
* MRuby: 0
* Nim: 1 (new function names)
* OCaml: 0
* Python: 0
* Rust: 0
* Rust-Cargo: 0


Samples that currently does not build, but did build before
-----------------------------------------------------------

* GCCGo, after the sdl2 package was changed (possibly due to gccgo supporting an older version of the Go language than the Go compiler)
* Rust and Rust-Cargo (several errors that are not related to sdl2 nor the sample code)


General information
----------------------

* Alexander F Rødseth, 2015
* MIT licensed

Pull requests are welcome!
