.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo --- Nim ---

main: main.nim
	nim c main.nim

run: msg main
	./main

clean:
	rm -f main

fullclean: clean
	rm -rf nimcache
