.PHONY: all clean fullclean msg sdlada run

SDLADA_PATH = /usr/share/gpr/sdlada.gpr
MAIN_GPR = main.gpr
MAIN_FILES = main.adb $(MAIN_GPR)

all: msg main

msg:
	@echo '--- Ada ---'

sdlada:
	@test -f $(SDLADA_PATH) || (echo 'error: sdlada must be installed ($(SDLADA_PATH) must exist)' && exit 1)

main: sdlada $(MAIN_FILES)
ifeq ($(shell uname -s), Darwin)
	sed -i '' 's/sdlada/SDLAda/g' $(MAIN_GPR)
else
	sed -i 's/SDLAda/sdlada/g' $(MAIN_GPR)
endif
	gprbuild -largs $(shell pkg-config sdl2 SDL2_image --libs) $^

run: msg main
	time ./main

clean:
	rm -f main *.o *.ali *.stderr *.stdout *.bexch *__*.a*

fullclean: clean
