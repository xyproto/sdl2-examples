FROM ubuntu:18.10

# Update and install packages
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y -q build-essential freeglut3-dev git libc-dev libglew-dev libsdl2-dev make mesa-common-dev python3 apt-utils libopenal-dev libvulkan-dev libglm-dev libsdl2-mixer-dev && \
    apt-get update && \
    apt-get clean

# Install sdl2-examples, but don't keep the git checkout in the docker image
RUN git clone https://github.com/xyproto/sdl2-examples

RUN cd sdl2-examples && \
    make -C c11 && \
    make -C c++98 && \
    make -C c++11
