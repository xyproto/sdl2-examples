.PHONY: all msg clean fullclean main

all: msg main

msg:
	@echo '--- Haskell ---'

main: src/Lib.hs app/Main.hs
	stack build

run: msg
	time stack run

clean:
	stack clean

fullclean: clean
