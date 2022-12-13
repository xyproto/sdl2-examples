.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- Zig ---'

main: main.zig
	zig build-exe -lc -lSDL2 -OReleaseFast --name $@ $<

small: main.zig
	zig build-exe -lc -lSDL2 -OReleaseSmall --strip --name $@ $<
	-sstrip "$@"

debug: main.zig
	zig build-exe -lc -lSDL2 --name $@ $<

main.s: main.zig
	@# This only works after clearing the cache!
	@-rm -rf zig-cache
	zig build-exe -lc -lSDL2 -OReleaseFast -femit-asm $<

asm: main.s

run: msg main
	time ./main

clean:
	rm -f main small *.o *.s

fullclean: clean
	rm -rf zig-cache
