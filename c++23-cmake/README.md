SDL2, C++23 and CMake
=====================

Requirements
------------

* CMake
* Compiler that supports C++23 (recent versions of `g++` or `clang++`, like GCC 11, supports `-std=c++2b`)
* SDL2

One way of building with C++23, SDL2 and CMake
----------------------------------------------

    cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
    make -C build

Running
-------

    build/main

Cleaning up the binary file and build directory
-----------------------------------------------

    rm -rf build/
