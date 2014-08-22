project(main)
aux_source_directory(. SRC_LIST)
add_executable(${PROJECT_NAME} ${SRC_LIST})

# CMake
cmake_minimum_required(VERSION 2.8)

# C++11
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

# SDL2
include(FindPkgConfig)
pkg_search_module(SDL2 REQUIRED sdl2)
include_directories(${SDL2_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} ${SDL2_LIBRARIES})
