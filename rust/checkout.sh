#!/bin/sh
rm -rf sdl2
git clone https://github.com/AngryLawyer/rust-sdl2.git sdl2
#cd sdl2
#git checkout 6025d7493ba461fffff254bae770868ec21288bd
#cd ..
make -C sdl2
