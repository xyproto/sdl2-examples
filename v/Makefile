.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- V ---'

main: main.v
	v -o main main.v

run: msg main
	time ./main

vrun: main.v
	v run main.v

clean:
	rm -f main

fullclean: clean
