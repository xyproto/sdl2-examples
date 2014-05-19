all:
	gcc -O3 -std=c99 `pkg-config sdl2 --libs --cflags` main.c -o main

small:
	gcc -Os -std=c99 `pkg-config sdl2 --libs --cflags` main.c -o main
	strip main
	sstrip main

debug:
	gcc -O0 -g -std=c99 `pkg-config sdl2 --libs --cflags` main.c -o main

clean:
	rm -f main
