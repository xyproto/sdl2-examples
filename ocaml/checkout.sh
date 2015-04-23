#!/bin/bash
[[ -d OCamlSDL2 ]] || git clone -q https://github.com/fccm/OCamlSDL2
cp OCamlSDL2/src/Makefile.config.unix OCamlSDL2/src/Makefile.config
