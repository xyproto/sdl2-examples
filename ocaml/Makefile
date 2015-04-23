.PHONY: all msg run clean fullclean

all: msg main

msg:
	@echo --- OCaml ---

sdl2: OCamlSDL2
	@echo -n '[OCamlSDL2] Compiling...'
	@make -s -C OCamlSDL2/src gen
	@make -s -C OCamlSDL2/src dep
	@make -s -C OCamlSDL2/src opt byte 2> /dev/null
	@echo 'ok'

OCamlSDL2:
	@echo -n '[OCamlSDL2] Checking out...'
	@./checkout.sh > /dev/null
	@echo 'ok'

main: sdl2 main.ml
	@# Use OCamlSDL2/src or /usr/lib/ocaml as the extra include directory
	@# depending on if the former directory exists or not. Avoids a warning.
	@echo -n '[main] Compiling...'
	@[ -e OCamlSDL2/src ] && ocamlopt -g -I OcamlSDL2/src sdl2.cmxa main.ml -o main || ocamlopt -g -I /usr/lib/ocaml sdl2.cmxa main.ml -o main
	@echo 'ok'

run: msg main
	time ./main

clean:
	rm -f main *.o *.cmx *.cmi

fullclean: clean
	rm -rf OcamlSDL2
