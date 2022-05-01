.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- FreeBASIC ---'

main: main.bas
	@echo -n '[main] Compiling...'
	@fbc main.bas
	@echo 'ok'

run: msg main
	time ./main

fullclean:
clean:
	rm -f *.o main
