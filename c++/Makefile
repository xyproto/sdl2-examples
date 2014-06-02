CPP=g++
CPPFLAGS=$(shell pkg-config sdl2 --libs --cflags) -std=c++0x

all:
	${CPP} -O3 ${CPPFLAGS} main.cpp -o main

small:
	${CPP} -Os ${CPPFLAGS} main.cpp -o main
	strip main
	sstrip main

debug:
	${CPP} -O0 -g ${CPPFLAGS} main.cpp -o main

run:
	@echo C++
	./main

clean:
	rm -f main
