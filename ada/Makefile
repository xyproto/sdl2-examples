.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- Ada ---'

main:
	alr update
	alr build

run: msg main
	time ./main

clean:
	rm -f main *.o

fullclean: clean
