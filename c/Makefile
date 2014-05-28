CC=gcc
CFLAGS=-std=c99 `pkg-config sdl2 --libs --cflags`

all:
	${CC} -O3 ${CFLAGS} main.c -o main

small:
	${CC} -Os ${CFLAGS} main.c -o main
	strip main
	sstrip main

debug:
	${CC} -O0 -g ${CFLAGS} main.c -o main

clean:
	rm -f main
