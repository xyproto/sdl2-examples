.PHONY: all msg clean fullclean

# Look for libraries in /usr/lib32 when linking if on i386, i686 or x86_32
ifeq ($(findstring $(shell uname -m),i386 i686 x86_32),)
LDFLAGS+=-L/usr/lib32
endif

all: msg main

msg:
	@echo '--- i386 ---'

main:
	nasm -felf32 main.asm -o main.o
	ld -m elf_i386 main.o -o main -dynamic-linker /lib/ld-linux.so.* -lc -lSDL2 $(LDFLAGS)

run: msg main
	time ./main

clean:
	rm -f main main.o

fullclean: clean
