.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- Odin ---'

main: main.odin
	odin build main.odin -file -out:main

run: msg main
	time ./main

odinrun: main.odin
	odin run main.odin -file

clean:
	rm -f main

fullclean: clean
