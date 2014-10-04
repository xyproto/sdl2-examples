Requirements
------------

CMake, compiler that supports C++ (recent version of g++ or clang++), SDL2 and make.


One way of building with C++11, SDL2, CMake and make
----------------------------------------------------

mkdir -p build
cd build
cmake ..
make
cd ..
mv build/main .


To clean up the binary file and build directory
-----------------------------------------------

rm -rf build/ main

