CC?=gcc
CFLAGS?=$(shell pkg-config sdl2 --cflags --libs) -std=c99

.PHONY: clean fullclean

all: main

main: main.c
	${CC} -O3 main.c ${CFLAGS} -o main

small: main.c
	${CC} -Os main.c ${CFLAGS} -o main
	strip main
	sstrip main

debug: main.c
	${CC} -O0 -g main.c ${CFLAGS} -o main

asm: main.asm

main.asm: main.c
	${CC} -Os -S main.c ${CFLAGS} -o main.asm

run: main
	@echo --- C ---
	./main

clean:
	rm -f main

fullclean: clean
