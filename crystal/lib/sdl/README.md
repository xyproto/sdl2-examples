# SDL2 bindings for Crystal

Write SDL2 games in Crystal. Support for desktop applications for now, but
Android shouldn't be hard to achieve.

Most of the bindings have been implemented, though most haven't been verified,
yet. Please see the `samples` directory for examples, ported from the tutorials
found at <http://lazyfoo.net/tutorials/SDL/index.php>. You are welcome to port
more samples, and the necessary corrections!

## Requirements

- SDL2 is required;
- Optional bindings for `SDL2_Mixer`, `SDL2_Image` and `SDL2_TTF`;
- Crystal > 0.22.0 is required for `SDL2_Mixer` to work correctly.
