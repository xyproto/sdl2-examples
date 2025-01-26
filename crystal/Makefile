.PHONY: all msg sdl2 clean fullclean

all: msg bin/main

msg:
	@echo '--- Crystal ---'

bin/main: shard.yml main.cr
	@echo -n '[main] Compiling...'
	@shards -q build
	@echo 'ok'

run: msg bin/main
	time ./bin/main

clean:
	rm -f bin/main

fullclean: clean
	rm -rf bin lib
