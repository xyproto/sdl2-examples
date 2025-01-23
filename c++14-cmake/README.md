SDL2, C++14 and CMake
=====================

Requirements
------------

* CMake
* Compiler that supports C++14 (recent version of `g++` or `clang++`)
* SDL2

One way of building with C++14, SDL2 and CMake
----------------------------------------------

    cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
    make -C build


Running
-------

    build/main

Cleaning up the binary file and build directory
-----------------------------------------------

    rm -rf build/
