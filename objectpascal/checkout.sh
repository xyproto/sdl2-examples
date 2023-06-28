#!/bin/bash
[[ -d SDL2 ]] || git clone -q https://git@github.com/de-signer/sdl2-fpc SDL2
[[ -d SDL2/SDL2 ]] && (mv SDL2/SDL2 temp; rm -rf SDL2; mv temp SDL2; chmod -x SDL2/*)
true
