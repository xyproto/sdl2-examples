CXX?=c++
CXXFLAGS?=$(shell pkg-config sdl2 --libs --cflags) -std=c++98 -Wall -pedantic -Werror -Wshadow -Wstrict-aliasing -Wstrict-overflow

.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo --- C++98 ---

main: main.cpp
	${CXX} ${CXXFLAGS} -O3 -o $@ $<

small: main.cpp
	${CXX} ${CXXFLAGS} -Os -o main $<
	-strip main
	-sstrip main

debug: main.cpp
	${CXX} ${CXXFLAGS} -O0 -g -o main $<

asm: main.asm

main.asm: main.cpp
	${CC} ${CFLAGS} -S -o main.asm $<

run: msg main
	time ./main

clean:
	rm -f main *.o main.asm

fullclean: clean
