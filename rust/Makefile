all: main.rs sdl2/build/lib/libsdl2.rlib
	rustc -o main -L sdl2/build/lib main.rs

run:
	@echo --- Rust ---
	./main

sdl2/build/lib/libsdl2.rlib:
	./checkout.sh

clean:
	-make -C sdl2 clean
	rm -f main
