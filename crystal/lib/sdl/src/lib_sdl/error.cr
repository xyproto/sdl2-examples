lib LibSDL
  fun set_error = SDL_SetError(fmt : Char*, ...)
  fun get_error = SDL_GetError() : Char*
  fun clear_error = SDL_ClearError()
end
