# C# + SDL2 example

SDL2 must either be installed, or be available as a shared library.

## Arch Linux

### Building and running with Mono

Building:

    msc /unsafe main.cs

Running:

    mono main.exe

### Building and running a native executable

`mkbundle` comes with `mono`.

Building:

Either run `make` or use a command similar to the one in the `Makefile`.

Running:

    ./main

### Building with `csc` and running with Wine

This builds a Windows GUI executable.

Building:

    csc /target:winexe /unsafe main.cs

Running:

    wine main.exe

## Windows

### Building

Either place csc.exe in the `PATH` and build with that, using:

    csc /target:winexe /unsafe main.cs

Or use the full path, for example:

    C:\Windows\Microsoft.NET\Framework\v3.5\csc /target:winexe /unsafe main.cs

### SDL2

`SDL2.dll` is included (zlib license). It can also be downloaded from here: https://www.libsdl.org/download-2.0.php

This text came together with the `SDL2.dll` binary:

```
The Simple DirectMedia Layer (SDL for short) is a cross-platform library
designed to make it easy to write multi-media software, such as games
and emulators.

The Simple DirectMedia Layer library source code is available from:
https://www.libsdl.org/

This library is distributed under the terms of the zlib license:
http://www.zlib.net/zlib_license.html
```
