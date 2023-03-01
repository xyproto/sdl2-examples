cmake_minimum_required(VERSION 3.0)

set(CMAKE_CXX_STANDARD 17)

project(java)

find_package(Java REQUIRED)
include(UseJava)

find_package(JNI REQUIRED)
find_package(SDL2 REQUIRED)

add_executable(main main.cpp)
target_include_directories(main PRIVATE ${JNI_INCLUDE_DIRS} ${SDL2_INCLUDE_DIRS})
target_link_libraries(main PRIVATE ${JNI_LIBRARIES} ${SDL2_LIBRARIES})
target_compile_definitions(main PRIVATE PROJECT_NAME="${PROJECT_NAME}")

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/Main.class
  COMMAND ${Java_JAVAC_EXECUTABLE} -d ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/Main.java
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/Main.java
)
add_custom_target(Main ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/Main.class)

install(TARGETS main DESTINATION bin)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/Main.class DESTINATION share/${PROJECT_NAME})
