SDL2, C++23 and CMake
=====================

Requirements
------------

* CMake
* Compiler that supports C++23 (recent versions of `g++` or `clang++`, like GCC 11, supports `-std=c++2b`)
* SDL2
* ninja (or `make`, just drop the `-GNinja` flag and build with `make`)

One way of building with C++23, SDL2, CMake and Ninja
-----------------------------------------------------

    cmake -S . -B build -G Ninja
    ninja -C build

Running
-------

    build/main

Cleaning up the binary file and build directory
-----------------------------------------------

    rm -rf build/
