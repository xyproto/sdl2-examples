.PHONY: all clean fullclean msg sdl2

all: msg main

msg:
	@echo '--- Nim ---'

sdl2:
	@-if [ ! -n "$$(find ~/.nimble/pkgs -maxdepth 1 -type d -name "sdl2-*" | head -1)" ]; then nimble install -y sdl2; fi

main: sdl2 main.nim
	nim -d:release c main.nim

run: msg main
	time ./main

clean:
	rm -rf main nimcache

fullclean: clean
	rm -rf sdl2
