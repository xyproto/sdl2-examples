cmake_minimum_required(VERSION 3.26)
project(main)

# Enable Objective-C support
enable_language(OBJC)

aux_source_directory(. SRC_LIST)
add_executable(${PROJECT_NAME} ${SRC_LIST})
target_include_directories(${PROJECT_NAME} PRIVATE ../include)

# Remove C++23 and add Objective-C specific flags
target_compile_options(${PROJECT_NAME} PRIVATE
    -fobjc-arc  # Enable ARC (Automatic Reference Counting)
)

target_compile_definitions(${PROJECT_NAME} PRIVATE IMGDIR="../img/")

# Add Foundation framework (required for Objective-C)
find_library(FOUNDATION_LIBRARY Foundation)
target_link_libraries(${PROJECT_NAME} PRIVATE ${FOUNDATION_LIBRARY})

# For macOS M2 systems with SDL2 installed via Homebrew
list(APPEND CMAKE_MODULE_PATH "/usr/local/lib/cmake/SDL2" "/opt/homebrew/lib/cmake/SDL2")
list(APPEND CMAKE_PREFIX_PATH "/usr/local" "/opt/homebrew")

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