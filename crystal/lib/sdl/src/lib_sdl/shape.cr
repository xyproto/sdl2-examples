lib LibSDL
  NONSHAPEABLE_WINDOW = -1
  INVALID_SHAPE_ARGUMENT = -2
  WINDOW_LACKS_SHAPE = -3

  enum ShapeMode
    Default
    BinarizeAlpha
    ReverseBinarizeAlpha
    ColorKey
  end

  union WindowShapeParams
    binarizationCutoff : UInt8
    colorKey : Color
  end

  struct WindowShapeMode
    mode : ShapeMode
    parameters : WindowShapeParams
  end

  fun create_shaped_window = SDL_CreateShapedWindow(title : Char*, x : UInt, y : UInt, w : UInt, h : UInt, flags : UInt32) : Window*
  fun is_shaped_window = IsShapedWindow(window : Window*) : Bool

  # SDL_SHAPEMODEALPHA(mode) (mode == ShapeModeDefault || mode == ShapeModeBinarizeAlpha || mode == ShapeModeReverseBinarizeAlpha)

  fun set_window_shape = SDL_SetWindowShape(window : Window*, shape : Surface*, shape_mode : WindowShapeMode*) : Int
  fun get_shaped_window_mode = SDL_GetShapedWindowMode(window : Window*, shape_mode : WindowShapeMode*) : Int
end
