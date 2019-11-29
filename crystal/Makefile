.PHONY: all msg sdl2 clean fullclean

all: msg main

msg:
	@echo '--- Crystal ---'

main: sdl.cr main.cr
	@echo -n '[main] Compiling...'
	@crystal build main.cr
	@echo 'ok'

sdl.cr: checkout.sh
	@echo -n '[SDL2 for Crystal] Checking out...'
	@./checkout.sh > /dev/null
	@echo 'ok'

run: msg main
	time ./main

clean:
	rm -f main

fullclean: clean
	rm -rf sdl.cr
