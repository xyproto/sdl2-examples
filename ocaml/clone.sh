#!/bin/bash
[[ -d OCamlSDL2 ]] || git clone -q https://github.com/fccm/OCamlSDL2
cp OCamlSDL2/src/Makefile.config.unix OCamlSDL2/src/Makefile.config
sed 's/\*\*/*/g' -i OCamlSDL2/src/sdlpower.ml

# noalloc fix
for name in texture timer; do
  sed 's/"noalloc"/\[@@noalloc\]/g' -i OCamlSDL2/src/sdl$name.ml
done

# lowercase fix
for name in blendMode render keymod keycode hat mouse; do
  sed 's/String.lowercase /String.lowercase_ascii /g' -i OCamlSDL2/src/sdl$name.ml
done

# uppercase fix
for name in pixelFormat scancode; do
  sed 's/String.uppercase /String.uppercase_ascii /g' -i OCamlSDL2/src/sdl$name.ml
done

sed 's/(int32[^_]/(int32_t/g' -i OCamlSDL2/src/sdlsurface_stub.c
sed 's/[^U_]int32[^_]/int32_t/g' -i OCamlSDL2/src/sdlsurface_stub.c
