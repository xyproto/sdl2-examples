SDL2, C++17 and CMake
=====================

Requirements
------------

* CMake
* Compiler that supports C++17 (recent version of `g++` or `clang++`)
* SDL2
* ninja (or `make`, just drop the `-GNinja` flag and build with `make`)

One way of building with C++17, SDL2, CMake and Ninja
-----------------------------------------------------

    mkdir -p build
    cd build
    cmake -GNinja ..
    ninja
    cd ..

Running
-------

    build/main

Cleaning up the binary file and build directory
-----------------------------------------------

    rm -rf build/
