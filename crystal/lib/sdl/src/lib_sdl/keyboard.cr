require "./keycode"

lib LibSDL
  struct Keysym
    scancode : Scancode
    sym : Keycode
    mod : Keymod
    unused : UInt32
  end

  fun get_keyboard_focus = SDL_GetKeyboardFocus() : Window*
  fun get_keyboard_state = SDL_GetKeyboardState(numkeys : Int*) : UInt8*
  fun get_mod_state = SDL_GetModState() : Keymod
  fun set_mod_state = SDL_SetModState(modstate : Keymod)
  fun get_key_from_scancode = SDL_GetKeyFromScancode(scancode : Scancode) : Keycode
  fun get_scancode_from_key = SDL_GetScancodeFromKey(key : Keycode) : Scancode
  fun get_scancode_name = SDL_GetScancodeName(scancode : Scancode) : Char*
  fun get_scancode_from_name = SDL_GetScancodeFromName(name : Char*) : Scancode
  fun get_key_name = SDL_GetKeyName(key : Keycode ) : Char*
  fun get_key_from_name = SDL_GetKeyFromName(name : Char*) : Keycode
  fun start_text_input = SDL_StartTextInput()
  fun is_text_input_active = SDL_IsTextInputActive() : Bool
  fun stop_text_input = SDL_StopTextInput()
  fun set_text_input_rect = SDL_SetTextInputRect(rect : Rect*)
  fun has_screen_keyboard_support = SDL_HasScreenKeyboardSupport() : Bool
  fun is_screen_keyboard_shown = SDL_IsScreenKeyboardShown(window : Window*) : Bool
end
