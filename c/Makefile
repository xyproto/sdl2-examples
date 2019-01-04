CC?=gcc
CFLAGS?=$(shell pkg-config sdl2 --cflags --libs) -std=c11 -Wall -pedantic -Werror -Wshadow -Wstrict-aliasing -Wstrict-overflow

.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo --- C ---

main: main.c
	${CC} main.c ${CFLAGS} -O3 -o main

small: main.c
	${CC} main.c ${CFLAGS} -Os -o main
	strip main
	sstrip main

debug: main.c
	${CC} main.c ${CFLAGS} -O1 -g -o main

asm: main.asm

main.asm: main.c
	${CC} main.c ${CFLAGS} -S -o main.asm

run: msg main
	time ./main

clean:
	rm -f main *.o

fullclean: clean
