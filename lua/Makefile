.PHONY: all msg clean fullclean

LUA_VERSION ?= 5.3

all: msg
	@# Nothing to build
	@true

msg:
	@echo '--- Lua ---'

run: msg main.lua
	time lua$(LUA_VERSION) main.lua

clean:
	@# Nothing to clean
	@true

fullclean: clean
