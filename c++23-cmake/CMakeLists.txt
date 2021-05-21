cmake_minimum_required(VERSION 3.12)

project(main)

aux_source_directory(. SRC_LIST)
add_executable(${PROJECT_NAME} ${SRC_LIST})

# This can probably be used with GCC 12 and above
#target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_23)

# These three lines are probably needed for GCC 11 and below
set(CMAKE_CXX_STANDARD 23)
set(CMaKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++2b")

target_include_directories(${PROJECT_NAME} PRIVATE ../include)
target_compile_definitions(${PROJECT_NAME} PRIVATE IMGDIR="../img/")

# The modern way, using cmake config files
#find_package(SDL2 REQUIRED)
#target_link_libraries(${PROJECT_NAME} PRIVATE SDL2::SDL2 SDL2::SDL2main)

# The trusty old way, using pkg-config
include(FindPkgConfig)
pkg_search_module(SDL2 REQUIRED sdl2)
target_include_directories(${PROJECT_NAME} PRIVATE ${SDL2_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} PRIVATE ${SDL2_LIBRARIES})
