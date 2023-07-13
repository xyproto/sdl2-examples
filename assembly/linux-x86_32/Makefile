.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- x86_32 ---'

main:
	nasm -felf32 main.asm -o main.o
	ld main.o -o main -dynamic-linker /lib/ld-linux.so.* -lc -lSDL2

run: msg main
	./main

clean:
	rm -f main main.o

fullclean: clean
