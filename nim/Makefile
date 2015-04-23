.PHONY: all clean fullclean

all: main

main: main.nim
	nim c main.nim

run: main
	@echo --- Nim ---
	./main

clean:
	rm -f main

fullclean: clean
	rm -rf nimcache
