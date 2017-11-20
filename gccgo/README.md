NOTE! This sample currently does not build.
Probably because the go-sdl2 package uses the syntax for a later version of Go than gccgo supports.

* Install the SDL2 library and headers.

* Install the latest version of Go.

* Install the latest version of GCC + gccgo.

* Install the sdl2 package:
  go get github.com/veandco/go-sdl2/sdl

* For OS X, remember to install the Development Libraries (place the SDL2 framework in /Library/Frameworks)

* Build:
  make
 
* Run:
  ./main

NOTE: This sample may not build, for some configurations.
