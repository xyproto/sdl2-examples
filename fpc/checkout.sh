#!/bin/sh
rm -rf SDL2
git clone https://bitbucket.org/p_daniel/sdl-2-for-free-pascal-compiler.git SDL2
mv SDL2/SDL2 temp
rm -rf SDL2
mv temp SDL2
chmod -x SDL2/*
