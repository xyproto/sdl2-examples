CC?=gcc
CFLAGS?=$(shell pkg-config sdl2 --cflags --libs) -std=c99

.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo --- C ---

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

run: msg main
	time ./main

clean:
	rm -f main

fullclean: clean
