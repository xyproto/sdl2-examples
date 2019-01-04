CXX?=c++
CXXFLAGS?=$(shell pkg-config sdl2 --libs --cflags) -std=c++98 -Wall -pedantic -Werror -Wshadow -Wstrict-aliasing -Wstrict-overflow

.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo --- C++ ---

main: main.cpp
	${CXX} -O3 ${CXXFLAGS} -o $@ $<

small: main.cpp
	${CXX} -Os ${CXXFLAGS} -o main $<
	strip main
	sstrip main

debug: main.cpp
	${CXX} -O0 -g ${CXXFLAGS} -o main $<

asm: main.asm

main.asm: main.cpp
	${CC} ${CFLAGS} -S -o main.asm $<

run: msg main
	time ./main

clean:
	rm -f main *.o main.asm

fullclean: clean
