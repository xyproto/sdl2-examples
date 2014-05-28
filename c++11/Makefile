CPP=g++
CPPFLAGS=-std=c++11 `pkg-config sdl2 --libs --cflags`

all:
	${CPP} -O3 ${CPPFLAGS} main.cpp -o main

small:
	${CPP} -Os ${CPPFLAGS} main.cpp -o main
	strip main
	sstrip main

debug:
	${CPP} -O0 -g ${CPPFLAGS} main.cpp -o main

run:
	@echo C++11
	./main

clean:
	rm -f main
