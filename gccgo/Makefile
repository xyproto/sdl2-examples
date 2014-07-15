CFLAGS?=$(shell pkg-config sdl2 --cflags --libs)

all:
	go build -compiler gccgo -gccgoflags '-O3 ${CFLAGS}' -o main

small:
	go build -compiler gccgo -gccgoflags '-Os ${CFLAGS}' -o main
	sstrip main

debug:
	go build -compiler gccgo -gccgoflags '-O0 -g ${CFLAGS}' -o main

static:
	go build -compiler gccgo -gccgoflags '-O2 ${CFLAGS} -static' -o main

run:
	@echo --- GccGo ---
	./main

clean:
	go clean
