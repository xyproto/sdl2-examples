lib LibSDL
  type Cursor = Void

  enum SystemCursor
    ARROW
    IBEAM
    WAIT
    CROSSHAIR
    WAITARROW
    SIZENWSE
    SIZENESW
    SIZEWE
    SIZENS
    SIZEALL
    NO
    HAND

    NUM_CURSORS
  end

  BUTTON_LEFT   = 1
  BUTTON_MIDDLE = 2
  BUTTON_RIGHT  = 3
  BUTTON_X1     = 4
  BUTTON_X2     = 5
  BUTTON_LMASK  = 1 << (BUTTON_LEFT - 1)
  BUTTON_MMASK  = 1 << (BUTTON_MIDDLE - 1)
  BUTTON_RMASK  = 1 << (BUTTON_RIGHT - 1)
  BUTTON_X1MASK = 1 << (BUTTON_X1 - 1)
  BUTTON_X2MASK = 1 << (BUTTON_X2 - 1)

  fun get_mouse_focus = SDL_GetMouseFocus() : Window*
  fun get_mouse_state = SDL_GetMouseState(x : Int*, y : Int*) : UInt32
  fun get_relative_mouse_state = SDL_GetRelativeMouseState(x : Int*, y : Int*) : UInt32
  fun warp_mouse_in_window = SDL_WarpMouseInWindow(window : Window*, x : Int, y : Int)
  fun set_relative_mouse_mode = SDL_SetRelativeMouseMode(enabled : Bool) : Int
  fun get_relative_mouse_mode = SDL_GetRelativeMouseMode() : Bool
  fun create_cursor = SDL_CreateCursor(data : UInt8*, mask : UInt8*, w : Int, h : Int, hot_x : Int, hot_y : Int) : Cursor*
  fun create_color_cursor = SDL_CreateColorCursor(surface : Surface*, hot_x : Int, hot_y : Int) : Cursor*
  fun create_system_cursor = SDL_CreateSystemCursor(id : SystemCursor) : Cursor*
  fun set_cursor = SDL_SetCursor(cursor : Cursor*)
  fun get_cursor = SDL_GetCursor() : Cursor*
  fun get_default_cursor = SDL_GetDefaultCursor() : Cursor*
  fun free_cursor = SDL_FreeCursor(cursor : Cursor*)
  fun show_cursor = SDL_ShowCursor(toggle : Int) : Int
end
