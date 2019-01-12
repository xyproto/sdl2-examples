#!/bin/sh
docker build --no-cache -t cxx:ubuntu_18_10 .
docker run --rm -it --name cxx_ubuntu_interactive cxx:ubuntu_18_10 bash
