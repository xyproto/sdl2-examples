.PHONY: all msg sdl2 clean fullclean

all: msg main

msg:
	@echo '--- Crystal ---'

main: shard.yml src/main.cr
	@echo -n '[main] Compiling...'
	@shards build
	@echo 'ok'

run: msg main
	time ./bin/main

clean:
	rm -f bin/main

fullclean: clean
	rm -rf bin lib
