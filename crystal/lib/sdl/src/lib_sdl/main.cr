lib LibSDL
  fun main = SDL_main(argc : Int, argv : Char**) : Int

  {% if flag?(:android) || flag?(:ios) %}
    redefine_main(SDL_main) do |main|
      \{{main}}
    end
  {% end %}

  fun set_main_ready = SDL_SetMainReady()

  {% if flag?(:windows) %}
    fun SDL_RegisterApp(name : Char*, style : UInt32, hInst : Void*) : Int
    fun SDL_UnregisterApp()
  {% end %}
end
