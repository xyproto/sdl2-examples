#!/bin/sh
rm -rf SDL2
git clone https://github.com/danpla/sdl2-fpc SDL2
mv SDL2/SDL2 temp
rm -rf SDL2
mv temp SDL2
chmod -x SDL2/*
