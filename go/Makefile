all:
	gccgo -O3 `pkg-config sdl2 --libs --cflags` main.go -o main

small:
	gccgo -Os `pkg-config sdl2 --libs --cflags` main.go -o main
	strip main
	sstrip main

debug:
	gccgo -O0 -g `pkg-config sdl2 --libs --cflags` main.go -o main

clean:
	rm -f main
