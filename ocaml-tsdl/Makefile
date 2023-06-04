.PHONY: all msg run clean fullclean

all: msg main

msg:
	@echo --- OCaml ---

main: main.ml
	@echo -n '[main] Compiling...'
	ocamlfind ocamlopt -package tsdl -linkpkg -thread -o $@ $<
	@echo 'ok'

run: msg main
	time ./main

clean:
	rm -f main *.o *.cmx

fullclean: clean
	rm -f *.cmi
