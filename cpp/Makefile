all:
	g++ -O3 -std=c++0x `pkg-config sdl2 --libs --cflags` main.cpp -o main

small:
	g++ -Os -std=c++0x `pkg-config sdl2 --libs --cflags` main.cpp -o main
	strip main
	sstrip main

debug:
	g++ -O0 -g -std=c++0x `pkg-config sdl2 --libs --cflags` main.cpp -o main

clean:
	rm -f main
