Requirements
------------

Build and install OCamlSDL2. For instance:

´´´
git clone https://github.com/fccm/OCamlSDL2
cd OCamlSDL2/src
cp Makefile.config.unix Makefile.config
make gen
make dep
make opt byte
sudo make install DESTDIR=/usr/lib/ocaml
´´´

Now everything is ready to build and run.


Note
----

* Make sure the ocaml sdl2 library files in /usr/lib/ocaml are readable (+r).
* Making this sample similar to the other samples is a work in progress.
