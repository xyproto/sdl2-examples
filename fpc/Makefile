all: main.pas SDL2/SDL2.o
	fpc -FuSDL2 main.pas

SDL2/SDL2.pas:
	./checkout.sh

SDL2/SDL2.o: SDL2/SDL2.pas
	fpc $<

SDL2/SDL2_image.o: SDL2/SDL2_image.pas
	fpc $<

SDL2/SDL2_mixer.o: SDL2/SDL2_mixer.pas
	fpc $<

SDL2/SDL2_net.o: SDL2/SDL2_net.pas
	fpc $<

SDL2/SDL2_ttf.o: SDL2/SDL2_ttf.pas
	fpc $<

run:
	@echo Free Pascal Compiler
	./main

clean:
	rm -f *.o main link.res SDL2/*.o SDL2/*.ppu
