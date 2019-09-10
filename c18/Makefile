CC?=gcc
SDL2FLAGS=$(shell pkg-config sdl2 --cflags --libs)
CFLAGS?=-std=c18 -Wall -pedantic -Werror -Wshadow -Wstrict-aliasing -Wstrict-overflow

.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo --- C18 ---

main: main.c
	${CC} ${CFLAGS} -O3 -o $@ $< ${SDL2FLAGS}

small: main.c
	${CC} ${CFLAGS} -Os -o main $< ${SDL2FLAGS}
	-strip main
	-sstrip main

debug: main.c
	${CC} ${CFLAGS} -O1 -g -o main $< ${SDL2FLAGS}

asm: main.asm

main.asm: main.c
	${CC} ${CFLAGS} -S -o main.asm $< ${SDL2FLAGS}

run: msg main
	time ./main

clean:
	rm -f main *.o main.asm

fullclean: clean
