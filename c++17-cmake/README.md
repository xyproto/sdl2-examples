SDL2, C++17 and CMake
=====================

Requirements
------------

* CMake
* Compiler that supports C++17 (recent version of `g++` or `clang++`)
* SDL2

One way of building with C++17, SDL2 and CMake
----------------------------------------------

    cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
    make -C build

Running
-------

    build/main

Cleaning up the binary file and build directory
-----------------------------------------------

    rm -rf build/
