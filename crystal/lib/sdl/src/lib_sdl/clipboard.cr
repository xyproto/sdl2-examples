lib LibSDL
  fun set_clipboard_text = SDL_SetClipboardText(text : Char*) : Int
  fun get_clipboard_text = SDL_GetClipboardText() : Char*
  fun has_clipboard_text = SDL_HasClipboardText() : Bool
end
