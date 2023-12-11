all: build/main.kexe

build/sdl2.klib: sdl2.def
	mkdir -p build
	cinterop \
		-compiler-options -I/usr/local/include/SDL2 \
		-def sdl2.def \
		-o build/sdl2

build/main.kexe: build/sdl2.klib Main.kt
	kotlinc-native \
		-l build/sdl2.klib \
		-linker-options /opt/homebrew/lib/libSDL2.dylib \
		-o build/main \
		Main.kt

run: build/main.kexe
	build/main.kexe

clean:
	@-rm -rfv build sdl2-build
