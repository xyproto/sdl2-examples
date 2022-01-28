.PHONY: all clean fullclean msg2 run

PACKAGE_NAME := sdl2-examples
MAIN_FUNCTION := renderer-test

all: msg main

quicklisp.lisp:
	@curl -Os "https://beta.quicklisp.org/$@"

quicklisp/setup.lisp: quicklisp.lisp
	@sbcl \
	  --non-interactive \
	  --noprint \
	  --no-sysinit \
	  --no-userinit \
	  --load $< \
	  --eval "(quicklisp-quickstart:install :path \"quicklisp\")"

msg:
	@echo '--- Common Lisp ---'

main: quicklisp/setup.lisp main.lisp
	@echo -n '[main] Compiling...'
	@sbcl \
	  --non-interactive \
	  --noprint \
	  --no-sysinit \
	  --no-userinit \
	  --load quicklisp/setup.lisp \
	  --eval "(ql:quickload 'sdl2)" \
	  --eval "(ql:quickload 'cl-opengl)" \
	  --load main.lisp \
	  --eval "(sb-ext:save-lisp-and-die \"$@\" :executable t :toplevel '${PACKAGE_NAME}:${MAIN_FUNCTION})"
	@echo 'ok'

small: quicklisp/setup.lisp main.lisp
	@echo -n '[main] Compiling, with compression enabled...'
	@sbcl \
	  --non-interactive \
	  --noprint \
	  --no-sysinit \
	  --no-userinit \
	  --load quicklisp/setup.lisp \
	  --eval "(ql:quickload 'sdl2)" \
	  --eval "(ql:quickload 'cl-opengl)" \
	  --load main.lisp \
	  --eval "(sb-ext:save-lisp-and-die \"$@\" :executable t :compression 9 :toplevel '${PACKAGE_NAME}:${MAIN_FUNCTION})"
	@echo 'ok'

run: msg quicklisp/setup.lisp main.lisp
	@time sbcl \
	  --non-interactive \
	  --noprint \
	  --no-sysinit \
	  --no-userinit \
	  --load quicklisp/setup.lisp \
	  --eval "(ql:quickload 'sdl2)" \
	  --eval "(ql:quickload 'cl-opengl)" \
	  --load main.lisp \
	  --eval "(${PACKAGE_NAME}:${MAIN_FUNCTION})"

clean:
	@-rm -f main small *.fas *.o

fullclean: clean
	@-rm -fr quicklisp quicklisp.lisp
