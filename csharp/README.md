# C# + SDL2 example

SDL2 must either be installed, or be available as a shared library.

## Arch Linux

### Building and running with Mono

Building:

    msc /unsafe main.cs

Running:

    mono main.exe

### Building and running as a native executable

`mkbundle` comes with `mono`.

Building:

    make

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

## macOS

macOS on M1/M2/aarch64 is not currently supported (as of 2023-06-28).

Running `make run` will:

* Create a `main.exe` file, but for the wrong CPU.
* Bundle the `main.exe` file into a `main` executable, which will look correct when using `file`.

But running `./main` will output an error message a bit like this one:

     Error mapping file: mono_file_map_error failed file:./main length:0x4685824X offset:0x3944448X error:Invalid argument(0x16)

Here is the status of macOS/M1/M2/aarch64 support for Mono:

* https://github.com/mono/mono/issues/21092

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

## macOS

On a macOS with a M2 CPU, a main.exe and main file is built, but they did not run correctly here, neither directly, with mono or with wine. Pull requests are welcome.
