# C# + SDL2 example

## Arch Linux

The code can also be compiled with:

    csc /unsafe main.cs

And run with ie.:

    mono main.exe

SDL2 must be installed, or available as a shared library.

## Windows

First place SDL2.dll in this directly. SDL2.dll is included in the archive that can be downloaded here:
https://www.libsdl.org/download-2.0.php

Then either place csc.exe in the PATH and build with that, using:

    csc /unsafe main.cs

Or use the full path, for example:

    C:\Windows\Microsoft.NET\Framework\v3.5\csc /unsafe main.cs
