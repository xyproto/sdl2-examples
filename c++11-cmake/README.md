SDL2, C++11 and CMake
=====================

Requirements
------------

* CMake
* Compiler that supports C++ (recent version of g++ or clang++)
* SDL2
* ninja (or make, just drop the `-GNinja` flag and build with `make`)

One way of building with C++11, SDL2, CMake and Ninja
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

    rm -rf build/ main
