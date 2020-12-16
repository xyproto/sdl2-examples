CC?=cxx
SDL2FLAGS=$(shell pkg-config sdl2 --cflags --libs)
CFLAGS?=-std=c18 -Wall -pedantic -Werror -Wshadow -Wstrict-aliasing -Wstrict-overflow

.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- Zig ---'

main: main.zig
	zig build-exe -lc -lSDL2 -OReleaseFast main.zig

small: main.zig
	zig build-exe -lc -lSDL2 -OReleaseSmall --strip main.zig
	-strip main
	-sstrip main

debug: main.zig
	zig build-exe -lc -lSDL2 main.zig

main.s: main.zig
	@# This only works after clearing the cache!
	@-rm -rf zig-cache
	zig build-exe -lc -lSDL2 -OReleaseFast -femit-asm main.zig

asm: main.s

run: msg main
	time ./main

clean:
	rm -f main *.o *.s

fullclean: clean
	rm -rf zig-cache
