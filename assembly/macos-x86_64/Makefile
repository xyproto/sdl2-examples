.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- x86_64 ---'

main:
	nasm -fmacho64 main.asm -o main.o
	ld main.o -o main -lc -lSDL2

run: msg main
	time ./main

clean:
	rm -f main main.o

fullclean: clean
