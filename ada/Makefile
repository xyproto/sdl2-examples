.PHONY: all clean fullclean msg sdlada

all: msg main

msg:
	@echo '--- Ada ---'

sdlada:
	@test -f /usr/share/gpr/sdlada.gpr || (echo 'error: sdlada must be installed (/usr/share/gpr/sdlada.gpr must exist)' && exit 1)

main: sdlada main.adb main.gpr
	gprbuild -largs $(shell pkg-config sdl2 SDL2_image --libs)

run: msg main
	time ./main

clean:
	rm -f main *.o *.ali *.stderr *.stdout *.bexch *__*.a*

fullclean: clean
