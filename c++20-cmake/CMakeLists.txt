cmake_minimum_required(VERSION 3.12)

project(main)

aux_source_directory(. SRC_LIST)
add_executable(${PROJECT_NAME} ${SRC_LIST})

include_directories(../include)
add_definitions(-DIMGDIR="../img/")

# This may set gnu++2a
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 20)

# SDL2
include(FindPkgConfig)
pkg_search_module(SDL2 REQUIRED sdl2)
include_directories(${SDL2_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} ${SDL2_LIBRARIES})
