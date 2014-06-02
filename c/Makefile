CC=gcc
CFLAGS=$(shell pkg-config sdl2 --cflags --libs) -std=c99

all:
	${CC} -O3 main.c ${CFLAGS} -o main

small:
	${CC} -Os main.c ${CFLAGS} -o main
	strip main
	sstrip main

debug:
	${CC} -O0 -g main.c ${CFLAGS} -o main

run:
	@echo C
	./main

clean:
	rm -f main
