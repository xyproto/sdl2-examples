.PHONY: all clean build run

# Java setup
JAVA_HOME ?= $(shell if [ -x /usr/libexec/java_home ]; then /usr/libexec/java_home -v 22; elif [ -d /opt/homebrew/Cellar/openjdk/22.0.1 ]; then echo /opt/homebrew/Cellar/openjdk/22.0.1; else echo /usr/lib/jvm/java-22-openjdk; fi)
JAVA=$(JAVA_HOME)/bin/java
JAVAC=$(JAVA_HOME)/bin/javac
JAR=$(JAVA_HOME)/bin/jar

# Check Java version
JAVA_VERSION_CHECK=$(shell $(JAVA) -version 2>&1 | grep 'version "22' || echo "not found")
ifeq ($(JAVA_VERSION_CHECK), not found)
$(error JAVA_HOME needs to point to Java 22 or later)
endif

# Java options
JAVA_OPTS=--enable-preview --enable-native-access=ALL-UNNAMED
JAVAC_OPTS=--enable-preview --source 22

# Project setup
TARGET_JAR=main.jar
JAVA_FILES=Main.java
MAIN_CLASS=Main
PACKAGE_NAME=main
BUILD_DIR=build

# Detect OS and set library path
ifeq ($(shell uname), Darwin)
  SDL2_LIB_PATH=$(shell if [ -f /opt/homebrew/lib/libSDL2.dylib ]; then echo /opt/homebrew/lib/libSDL2.dylib; elif [ -f /usr/local/lib/libSDL2.dylib ]; then echo /usr/local/lib/libSDL2.dylib; else echo notfound; fi)
else ifeq ($(shell uname), Linux)
  SDL2_LIB_PATH=$(shell pkg-config --variable=libdir sdl2 2>/dev/null)/libSDL2.so
  ifeq ($(wildcard $(SDL2_LIB_PATH)),)
    SDL2_LIB_PATH=$(shell if [ -f /usr/lib/libSDL2.so ]; then echo /usr/lib/libSDL2.so; elif [ -f /usr/lib64/libSDL2.so ]; then echo /usr/lib64/libSDL2.so; else echo notfound; fi)
  endif
else ifeq ($(shell uname), FreeBSD)
  SDL2_LIB_PATH=/usr/local/lib/libSDL2.so
else ifeq ($(OS), Windows_NT)
  SDL2_LIB_PATH=SDL2.dll
else
  $(error Unsupported OS)
endif

# Check if SDL2 library exists
ifeq ($(SDL2_LIB_PATH), notfound)
  $(error SDL2 library not found. Please install SDL2 using your system's package manager)
endif

all: build

build: $(TARGET_JAR)

$(TARGET_JAR): $(JAVA_FILES)
	@mkdir -p "$(BUILD_DIR)/META-INF"
	@$(JAVAC) $(JAVAC_OPTS) -d "$(BUILD_DIR)" $(JAVA_FILES)
	@echo "Main-Class: $(MAIN_CLASS)" > "$(BUILD_DIR)/META-INF/MANIFEST.MF"
	@$(JAR) cmf "$(BUILD_DIR)/META-INF/MANIFEST.MF" $(TARGET_JAR) -C "$(BUILD_DIR)" .
	@rm -rf "$(BUILD_DIR)"

run:
	@$(JAVA) $(JAVA_OPTS) -Dsdl2.library.path=$(SDL2_LIB_PATH) -jar $(TARGET_JAR)

clean:
	@rm -f $(TARGET_JAR)