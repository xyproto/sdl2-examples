DerelictSDL2
============

Dynamic bindings to [SDL 2][1] version 2.0.5, [SDL2_image][2], [SDL2_mixer][3], [SDL2_ttf][4], and [SDL2_net][5] for the D Programming Language.

Please see the pages [Building and Linking Derelict][6] and [Using Derelict][7], in the Derelict documentation, for information on how to build DerelictSDL2 and load the SDL2 libraries at run time. In the meantime, here's some sample code.

```D
// This example shows how to import all of the DerelictSDL2 bindings. Of course,
// you only need to import the modules that correspond to the libraries you
// actually need to load.
import derelict.sdl2.sdl;
import derelict.sdl2.image;
import derelict.sdl2.mixer;
import derelict.sdl2.ttf;
import derelict.sdl2.net;

void main() {
    // This example shows how to load all of the SDL2 libraries. You only need
    // to call the load methods for those libraries you actually need to load.

    // Load the SDL 2 library.
    DerelictSDL2.load();

    // Load the SDL2_image library.
    DerelictSDL2Image.load();

    // Load the SDL2_mixer library.
    DerelictSDL2Mixer.load();

    // Load the SDL2_ttf library
    DerelictSDL2ttf.load();

    // Load the SDL2_net library.
    DerelictSDL2Net.load();

    // Now SDL 2 functions for all of the SDL2 libraries can be called.
    ...
}
```

[1]: http://www.libsdl.org/download-2.0.php
[2]: http://www.libsdl.org/projects/SDL_image/
[3]: http://www.libsdl.org/projects/SDL_mixer/
[4]: http://www.libsdl.org/projects/SDL_ttf/
[5]: http://www.libsdl.org/projects/SDL_net/
[6]: http://derelictorg.github.io/compiling.html
[7]: http://derelictorg.github.io/using.html



