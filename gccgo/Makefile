.PHONY: all clean debug main msg run small static 

CFLAGS?=$(shell pkg-config sdl2 --cflags --libs)

all: CFLAGS+=-O2
all: main

small: CFLAGS+=-Os
small: main
	sstrip main

debug: CFLAGS+=-O0 -g
debug: main

static: CFLAGS+=-O2 -static
static: main

main:
	go build -compiler gccgo -gccgoflags '${CFLAGS}' -o main

msg:
	@echo '--- GCCGO ---'

run:
	./main

clean:
	go clean
