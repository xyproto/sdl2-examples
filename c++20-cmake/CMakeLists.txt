cmake_minimum_required(VERSION 3.12)

project(main)

aux_source_directory(. SRC_LIST)
add_executable(${PROJECT_NAME} ${SRC_LIST})

target_include_directories(${PROJECT_NAME} PRIVATE ../include)
target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_20)

add_definitions(-DIMGDIR="../img/")

find_package(SDL2 REQUIRED)
target_link_libraries(${PROJECT_NAME} PRIVATE SDL2::SDL2 SDL2::SDL2main)
