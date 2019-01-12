#!/bin/sh
scriptdir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$scriptdir"
docker build --no-cache -t cxx:ubuntu_18_10 . && docker run --rm --name cxx_ubuntu cxx:ubuntu_18_10
