.PHONY: all main msg clean fullclean

all: msg main

msg:
	@echo '--- Gambas ---'

main: main.bas
	gbc3
	gba3 -o main

run: msg main
	time ./main

clean:
	rm -f main

fullclean: clean
