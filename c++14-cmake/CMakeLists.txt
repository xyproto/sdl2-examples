cmake_minimum_required(VERSION 3.26)

project(main)

aux_source_directory(. SRC_LIST)
add_executable(${PROJECT_NAME} ${SRC_LIST})

target_include_directories(${PROJECT_NAME} PRIVATE ../include)
target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_14)
target_compile_definitions(${PROJECT_NAME} PRIVATE IMGDIR="../img/")

# For macOS M2 systems with SDL2 installed via Homebrew
list(APPEND CMAKE_MODULE_PATH "/usr/local/lib/cmake/SDL2" "/opt/homebrew/lib/cmake/SDL2")
list(APPEND CMAKE_PREFIX_PATH "/usr/local" "/opt/homebrew")

# Try the modern way first
find_package(SDL2 QUIET)

if (SDL2_FOUND)
    message("Using SDL2 from find_package")
    target_include_directories(${PROJECT_NAME} PRIVATE ${SDL2_INCLUDE_DIRS})
    target_link_libraries(${PROJECT_NAME} PRIVATE ${SDL2_LIBRARIES})
else()
    # Fall back on pkg-config
    include(FindPkgConfig)
    pkg_search_module(SDL2 REQUIRED sdl2)
    message("Using SDL2 from pkg-config")
    target_include_directories(${PROJECT_NAME} PRIVATE ${SDL2_INCLUDE_DIRS})
    target_link_libraries(${PROJECT_NAME} PRIVATE ${SDL2_LIBRARIES})
endif()
