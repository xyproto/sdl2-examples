.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo --- Nim ---

main: main.nim
	nim c main.nim

run: msg main
	time ./main

clean:
	rm -rf main nimcache

fullclean: clean
