CXX?=g++
CXXFLAGS?=$(shell pkg-config sdl2 --libs --cflags)

all:
	${CXX} -O3 main.cpp ${CXXFLAGS} -o main

small:
	${CXX} -Os main.cpp ${CXXFLAGS} -o main
	strip main
	sstrip main

debug:
	${CXX} -O0 -g main.cpp ${CXXFLAGS} -o main

run:
	@echo C++
	./main

clean:
	rm -f main
