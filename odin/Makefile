.PHONY: all msg clean fullclean

all: msg odin

msg:
	@echo '--- Odin ---'

odin: main.odin
	odin build main.odin -file

run: msg odin
	time ./main.bin

odinrun: main.odin
	odin run main.odin -file

clean:
	rm -f ./main.bin

fullclean: clean
