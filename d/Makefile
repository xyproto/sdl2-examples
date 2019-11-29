.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- D ---'

main: source/app.d
	@echo -n '[main] Compiling...'
	@dub -q build
	@echo 'ok'

run: msg main
	time ./main

clean:
	rm -f main

fullclean: clean
