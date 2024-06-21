.PHONY: all clean build run

JAVA=$(JAVA_HOME)/bin/java
JAVAC=$(JAVA_HOME)/bin/javac
JAR=$(JAVA_HOME)/bin/jar

JAVA_VERSION_CHECK=$(shell $(JAVA) -version 2>&1 | grep 'version "22' || echo "not found")
ifeq ($(JAVA_VERSION_CHECK), not found)
export JAVA_HOME=/usr/lib/jvm/default
ifeq ($(shell uname), Darwin)
export JAVA_HOME=/opt/homebrew/Cellar/openjdk/22.0.1/
endif
endif

JAVA_VERSION_CHECK=$(shell $(JAVA) -version 2>&1 | grep 'version "22' || echo "not found")
ifeq ($(JAVA_VERSION_CHECK), not found)
$(error JAVA_HOME does not point to Java 22 or later)
endif

JAVA_OPTS=--enable-preview --enable-native-access=ALL-UNNAMED
JAVAC_OPTS=--enable-preview --source 22
TARGET_JAR=main.jar
JAVA_FILES=Main.java
MAIN_CLASS=Main
PACKAGE_NAME=main
BUILD_DIR=build

# Detect OS and set library path
ifeq ($(shell uname), Darwin)
  SDL2_LIB_PATH=/opt/homebrew/lib/libSDL2-2.0.0.dylib
else ifeq ($(shell uname), Linux)
  SDL2_LIB_PATH=/usr/lib/libSDL2.so
else ifeq ($(shell uname), FreeBSD)
  SDL2_LIB_PATH=/usr/local/lib/libSDL2.so
else ifeq ($(OS), Windows_NT)
  SDL2_LIB_PATH=SDL2.dll
else
  $(error Unsupported OS)
endif

all: build

build: $(TARGET_JAR)
$(TARGET_JAR): $(JAVA_FILES)
	@mkdir -p "$(BUILD_DIR)/META-INF"
	@$(JAVAC) $(JAVAC_OPTS) -d "$(BUILD_DIR)" $(JAVA_FILES)
	@echo "Main-Class: $(MAIN_CLASS)" > "$(BUILD_DIR)/META-INF/MANIFEST.MF"
	@$(JAR) cmf "$(BUILD_DIR)/META-INF/MANIFEST.MF" $(TARGET_JAR) -C "$(BUILD_DIR)" .
	@rm -r "$(BUILD_DIR)"

run:
	@$(JAVA) $(JAVA_OPTS) -Dsdl2.library.path=$(SDL2_LIB_PATH) -jar $(TARGET_JAR)

clean:
	@-rm -rf $(TARGET_JAR) "$(BUILD_DIR)"
