require "./lib_sdl/main"
require "./lib_sdl/audio"
require "./lib_sdl/clipboard"
#require "./lib_sdl/cpuinfo"
require "./lib_sdl/error"
require "./lib_sdl/events"
require "./lib_sdl/joystick"
require "./lib_sdl/gamecontroller"
#require "./lib_sdl/haptic"
require "./lib_sdl/hints"
require "./lib_sdl/messagebox"
require "./lib_sdl/mouse"
require "./lib_sdl/pixels"
require "./lib_sdl/power"
require "./lib_sdl/render"
require "./lib_sdl/rwops"
require "./lib_sdl/shape"
#require "./lib_sdl/system"
require "./lib_sdl/video"

@[Link("SDL2")]
lib LibSDL
  alias Bool = LibC::Int

  alias Char = LibC::Char
  alias Double = LibC::Double
  alias Float = LibC::Float
  alias Int = LibC::Int
  alias UInt = LibC::UInt
  alias Long = LibC::Long
  alias SizeT = LibC::SizeT

  VERSION = {% `pkg-config sdl2 --modversion`.strip %}
  MAJOR = {% VERSION.split('.')[0] %}
  MINOR = {% VERSION.split('.')[1] %}
  PATCH = {% VERSION.split('.')[2] %}

  INIT_TIMER          = 0x00000001
  INIT_AUDIO          = 0x00000010
  INIT_VIDEO          = 0x00000020
  INIT_JOYSTICK       = 0x00000200
  INIT_HAPTIC         = 0x00001000
  INIT_GAMECONTROLLER = 0x00002000
  INIT_EVENTS         = 0x00004000
  INIT_NOPARACHUTE    = 0x00100000
  INIT_EVERYTHING     = 0x00107231 # INIT_TIMER | INIT_AUDIO | INIT_VIDEO | INIT_EVENTS | INIT_JOYSTICK | INIT_HAPTIC | INIT_GAMECONTROLLER

  fun init = SDL_Init(flags : UInt32) : Int
  fun init_sub_system = SDL_InitSubSystem(flags : UInt32) : Int
  fun quit_sub_system = SDL_QuitSubSystem(flags : UInt32)
  fun was_init = SDL_WasInit(flags : UInt32) : UInt32
  fun quit = SDL_Quit()
end
