.PHONY: all clean fullclean msg2 run

all: msg main

quicklisp.lisp:
	@curl -Os 'https://beta.quicklisp.org/quicklisp.lisp'

quicklisp/setup.lisp: quicklisp.lisp
	@sbcl \
	  --non-interactive \
	  --noprint \
	  --no-sysinit \
	  --no-userinit \
	  --load quicklisp.lisp \
	  --eval "(quicklisp-quickstart:install :path \"quicklisp\")"
msg:
	@echo '--- Common Lisp ---'

main: quicklisp/setup.lisp
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
	  --eval "(sb-ext:save-lisp-and-die \"main\" :executable t :toplevel 'sdl2-examples:basic-test)"
	@echo 'ok'

run: msg quicklisp/setup.lisp
	@time sbcl \
	  --non-interactive \
	  --noprint \
	  --no-sysinit \
	  --no-userinit \
	  --load quicklisp/setup.lisp \
	  --eval "(ql:quickload 'sdl2)" \
	  --eval "(ql:quickload 'cl-opengl)" \
	  --load main.lisp \
	  --eval "(sdl2-examples:basic-test)"

clean:
	@-rm -f main

fullclean: clean
	@-rm -fr quicklisp quicklisp.lisp
