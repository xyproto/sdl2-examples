.PHONY: all msg sdl2 clean fullclean

all: msg main

msg:
	@echo --- FPC ---

main: sdl2 SDL2/SDL2.o main.pas
	@echo -n '[main] Compiling...'
	@fpc -FuSDL2 main.pas
	@echo 'ok'

sdl2: checkout.sh
	@echo -n '[SDL2 for FPC] Checking out...'
	@./checkout.sh > /dev/null
	@echo 'ok'

SDL2/SDL2.o: SDL2/SDL2.pas
	@fpc $<

SDL2/SDL2_image.o: SDL2/SDL2_image.pas
	@fpc $<

SDL2/SDL2_mixer.o: SDL2/SDL2_mixer.pas
	@fpc $<

SDL2/SDL2_net.o: SDL2/SDL2_net.pas
	@fpc $<

SDL2/SDL2_ttf.o: SDL2/SDL2_ttf.pas
	@fpc $<

run: msg main
	time ./main

clean:
	rm -f *.o main link.res SDL2/*.o SDL2/*.ppu

fullclean: clean
	rm -rf SDL2
