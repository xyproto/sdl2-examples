#!/bin/sh

rm -rf sdl2
git clone https://github.com/AngryLawyer/rust-sdl2.git sdl2
make -C sdl2
