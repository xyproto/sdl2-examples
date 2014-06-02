CPP?=g++
CPPFLAGS?=$(shell pkg-config sdl2 --libs --cflags) -std=c++11

all:
	${CPP} -O3 main.cpp ${CPPFLAGS} -o main

small:
	${CPP} -Os main.cpp ${CPPFLAGS} -o main
	strip main
	sstrip main

debug:
	${CPP} -O0 -g main.cpp ${CPPFLAGS} -o main

run:
	@echo C++11
	./main

clean:
	rm -f main
