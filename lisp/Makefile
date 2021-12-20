.PHONY: all clean fullclean msg2 run

all: msg main

quicklisp.lisp:
	curl -O 'https://beta.quicklisp.org/quicklisp.lisp'

quicklisp/setup.lisp: quicklisp.lisp
	sbcl \
	  --load quicklisp.lisp \
	  --eval "(quicklisp-quickstart:install :path \"quicklisp\")" \
	  --eval "(quit)"

msg:
	@echo '--- Common Lisp ---'

main: quicklisp quicklisp/setup.lisp
	@echo -n '[main] Compiling...'
	@sbcl \
	  --load quicklisp/setup.lisp \
	  --eval "(ql:quickload 'sdl2)" \
	  --eval "(ql:quickload 'cl-opengl)" \
	  --load main.lisp \
	  --eval "(sb-ext:save-lisp-and-die \"main\" :executable t :toplevel 'sdl2-examples:basic-test)" \
	  --eval "(quit)"
	@echo 'ok'

run: quicklisp/setup.lisp msg
	time sbcl \
	  --load quicklisp/setup.lisp \
	  --eval "(ql:quickload 'sdl2)" \
	  --eval "(ql:quickload 'cl-opengl)" \
	  --load main.lisp \
	  --eval "(sdl2-examples:basic-test)" \
	  --eval "(quit)"

clean:
	rm -f main

fullclean: clean
	rm -fr quicklisp quicklisp.lisp
