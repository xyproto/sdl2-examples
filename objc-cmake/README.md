SDL2, Objective-C and CMake
===========================

Requirements
------------

* CMake
* Compiler that supports Objective-C (like `clang`)
* SDL2

One way of building with Objective-C, SDL2 and CMake
----------------------------------------------------

    cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
    make -C build

Running
-------

    build/main

Cleaning up the binary file and build directory
-----------------------------------------------

    rm -rf build/
