all:
	gccgo -O3 -std=c99 `pkg-config sdl2 --libs --cflags` main.go -o main

small:
	gccgo -Os -std=c99 `pkg-config sdl2 --libs --cflags` main.go -o main
	strip main
	sstrip main

debug:
	gccgo -O0 -g -std=c99 `pkg-config sdl2 --libs --cflags` main.go -o main

clean:
	rm -f main
