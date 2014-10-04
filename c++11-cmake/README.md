SDL2, C++11 and CMake
=====================

Requirements
------------

* CMake
* Compiler that supports C++ (recent version of g++ or clang++)
* SDL2
* make (unless you choose a different target when building with cmake)

One way of building with C++11, SDL2, CMake and make
----------------------------------------------------

    mkdir -p build
    cd build
    cmake ..
    make
    cd ..
    mv build/main .

Cleaning up the binary file and build directory
-----------------------------------------------

    rm -rf build/ main

