.PHONY: all clean

all: main

main: main.ml
	ocamlopt -g -I /usr/lib/ocaml sdl2.cmxa main.ml -o main

run:
	@echo --- OCaml ---
	./main

clean:
	rm -f main *.o *.cmx *.cmi
