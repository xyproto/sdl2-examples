CC=gcc
CFLAGS=$(shell pkg-config sdl2 --cflags --libs) -std=c99

all:
	${CC} -O3 ${CFLAGS} main.c -o main

small:
	${CC} -Os ${CFLAGS} main.c -o main
	strip main
	sstrip main

debug:
	${CC} -O0 -g ${CFLAGS} main.c -o main

run:
	@echo C
	./main

clean:
	rm -f main
