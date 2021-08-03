module SDL
  # Internal macro to return either a pointer to a struct or return or null
  # pointer. This compiler gymnastic is mostly required to pass a reference
  # to Rect which may be nil.
  #
  # :nodoc:
  macro pointer_or_null(variable, type)
    if {{variable.id}}
      %copy = {{variable.id}}
      pointerof(%copy)
    else
      Pointer({{type.id}}).null
    end
  end
end

require "./lib_sdl"
require "./events"
require "./hint"
require "./renderer"
require "./window"
require "./screensaver"
require "./color"

module SDL
  class Error < Exception
    def initialize(name)
      super("#{name}: #{String.new(LibSDL.get_error)}")
    end
  end

  @[Flags]
  enum Init
    TIMER          = LibSDL::INIT_TIMER
    AUDIO          = LibSDL::INIT_AUDIO
    VIDEO          = LibSDL::INIT_VIDEO
    JOYSTICK       = LibSDL::INIT_JOYSTICK
    HAPTIC         = LibSDL::INIT_HAPTIC
    GAMECONTROLLER = LibSDL::INIT_GAMECONTROLLER
    EVENTS         = LibSDL::INIT_EVENTS
    NOPARACHUTE    = LibSDL::INIT_NOPARACHUTE
    EVERYTHING     = LibSDL::INIT_EVERYTHING
  end

  def self.init(flags : Init)
    ret = if @@initialized
            LibSDL.init(flags)
          else
            LibSDL.init_sub_system(flags)
          end
    if ret == 0
      @@initialized = true
    else
      raise Error.new("SDL_Init: failed")
    end
  end

  def self.init?(flag : Init)
    LibSDL.was_init(flag) == flag
  end

  def self.quit(flags : Init? = nil)
    if flags
      LibSDL.quit_sub_system(flags)
    else
      LibSDL.quit
    end
  end
end
