all: prepare cinterop kotlin-native

prepare:
	mkdir -p build

kotlin-native: prepare
	kotlinc-native \
		-o build/main \
		-l build/sdl.klib \
		-linker-options /opt/homebrew/lib/libSDL2.dylib \
		main.kt

cinterop: prepare
	cinterop \
		-o build/sdl \
		-def sdl.def \
		-compiler-options -I/usr/local/include/SDL2

run:
	build/main.kexe

clean:
	@if [ -d build ]; then rm -rf build; fi
	@if [ -d sdl-build ]; then rm -rf sdl-build; fi
