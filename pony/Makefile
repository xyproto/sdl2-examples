.PHONY: all msg clean fullclean

all: msg pony

msg:
	@echo --- pony ---

pony: main.pony
	ponyc

run: msg pony
	time ./pony

clean:
	rm -f pony

fullclean: clean
