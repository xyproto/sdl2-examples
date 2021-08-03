require "./error"
require "./video"
require "./keyboard"
require "./mouse"
require "./joystick"
require "./gamecontroller"
require "./gesture"
require "./touch"

lib LibSDL
  RELEASED = 0
  PRESSED  = 1

  enum EventType : UInt32
    FIRSTEVENT =  0

    # application

    QUIT = 0x100
    APP_TERMINATING
    APP_LOW_MEMORY
    APP_WILL_ENTER_BACKGROUND
    APP_DID_ENTER_BACKGROUND
    APP_WILL_ENTER_FOREGROUND
    APP_DID_ENTER_FOREGROUND

    # window

    WINDOW_EVENT = 0x200
    SYS_WM_EVENT

    # keyboard

    KEYDOWN = 0x300
    KEYUP
    TEXT_EDITING
    TEXT_INPUT

    # mouse

    MOUSE_MOTION = 0x400
    MOUSE_BUTTON_DOWN
    MOUSE_BUTTON_UP
    MOUSE_WHEEL

    # joystick

    JOY_AXIS_MOTION = 0x600
    JOY_BALL_MOTION
    JOY_HAT_MOTION
    JOY_BUTTON_DOWN
    JOY_BUTTON_UP
    JOY_DEVICE_ADDED
    JOY_DEVICE_REMOVED

    # game controller

    CONTROLLER_AXIS_MOTION = 0x650
    CONTROLLER_BUTTON_DOWN
    CONTROLLER_BUTTON_UP
    CONTROLLER_DEVICE_ADDED
    CONTROLLER_DEVICE_REMOVED
    CONTROLLER_DEVICE_REMAPPED

    # touch

    FINGER_DOWN = 0x700
    FINGER_UP
    FINGER_MOTION

    # gesture

    DOLLAR_GESTURE = 0x800
    DOLLAR_RECORD
    MULTI_GESTURE

    # clipboard
    CLIPBOARD_UPDATE = 0x900

    # drag n' drop
    DROP_FILE  = 0x1000

    # render
    RENDER_TARGETS_RESET = 0x2000

    # user custom
    USER_EVENT = 0x8000

    # maximum event number
    LAST_EVENT = 0xffff
  end

  struct CommonEvent
    type : EventType
    timestamp : UInt32
  end

  struct WindowEvent
    type : EventType
    timestamp : UInt32
    windowID : UInt32
    event : UInt8
    padding1 : UInt8
    padding2 : UInt8
    padding3 : UInt8
    data1 : Int32
    data2 : Int32
  end

  struct KeyboardEvent
    type : EventType
    timestamp : UInt32
    windowID : UInt32
    state : UInt8
    repeat : UInt8
    padding2 : UInt8
    padding3 : UInt8
    keysym : Keysym
  end

  TEXTEDITINGEVENT_TEXT_SIZE = 32

  struct TextEditingEvent
    type : EventType
    timestamp : UInt32
    windowID : UInt32
    text : Char[TEXTEDITINGEVENT_TEXT_SIZE]
    start : Int32
    length : Int32
  end

  TEXTINPUTEVENT_TEXT_SIZE = 32

  struct TextInputEvent
    type : EventType
    timestamp : UInt32
    windowID : UInt32
    text : Char[TEXTINPUTEVENT_TEXT_SIZE]
  end

  struct MouseMotionEvent
    type : EventType
    timestamp : UInt32
    windowID : UInt32
    which : UInt32
    state : UInt32
    x : Int32
    y : Int32
    xrel : Int32
    yrel : Int32
  end

  struct MouseButtonEvent
    type : EventType
    timestamp : UInt32
    windowID : UInt32
    which : UInt32
    button : UInt8
    state : UInt8
    clicks : UInt8
    padding1 : UInt8
    x : Int32
    y : Int32
  end

  struct MouseWheelEvent
    type : EventType
    timestamp : UInt32
    windowID : UInt32
    which : UInt32
    x : Int32
    y : Int32
  end

  struct JoyAxisEvent
    type : EventType
    timestamp : UInt32
    which : JoystickID
    axis : UInt8
    padding1 : UInt8
    padding2 : UInt8
    padding3 : UInt8
    value : Int16
    padding4 : UInt16
  end

  struct JoyBallEvent
    type : EventType
    timestamp : UInt32
    which : JoystickID
    ball : UInt8
    padding1 : UInt8
    padding2 : UInt8
    padding3 : UInt8
    xrel : Int16
    yrel : Int16
  end

  struct JoyHatEvent
    type : EventType
    timestamp : UInt32
    which : JoystickID
    hat : UInt8
    value : UInt8
    padding1 : UInt8
    padding2 : UInt8
  end

  struct JoyButtonEvent
    type : EventType
    timestamp : UInt32
    which : JoystickID
    button : UInt8
    state : UInt8
    padding1 : UInt8
    padding2 : UInt8
  end

  struct JoyDeviceEvent
    type : EventType
    timestamp : UInt32
    which : Int32
  end

  struct ControllerAxisEvent
    type : EventType
    timestamp : UInt32
    which : JoystickID
    axis : UInt8
    padding1 : UInt8
    padding2 : UInt8
    padding3 : UInt8
    value : Int16
    padding4 : UInt16
  end

  struct ControllerButtonEvent
    type : EventType
    timestamp : UInt32
    which : JoystickID
    button : UInt8
    state : UInt8
    padding2 : UInt8
    padding3 : UInt8
  end

  struct ControllerDeviceEvent
    type : EventType
    timestamp : UInt32
    which : Int32
  end

  struct TouchFingerEvent
    type : EventType
    timestamp : UInt32
    touchId : TouchID
    fingerId : FingerID
    x : Float
    y : Float
    dx : Float
    dy : Float
    pressure : Float
  end

  struct MultiGestureEvent
    type : EventType
    timestamp : UInt32
    touchId : TouchID
    dTheta : Float
    dDist : Float
    x : Float
    y : Float
    numFingers : UInt16
    padding : UInt16
  end

  struct DollarGestureEvent
    type : EventType
    timestamp : UInt32
    touchId : TouchID
    gestureId : GestureID
    numFingers : UInt32
    error : Float
    x : Float
    y : Float
  end

  struct DropEvent
    type : EventType
    timestamp : UInt32
    file : Char* # filename, must be freed with SDL_Free()
  end

  struct QuitEvent
    type : EventType
    timestamp : UInt32
  end

  #struct OSEvent
  #  type : EventType
  #  timestamp : UInt32
  #end

  struct UserEvent
    type : EventType
    timestamp : UInt32
    windowID : UInt32
    code : Int32
    data1 : Void*
    data2 : Void*
  end

  alias SysWMmsg = Void

  struct SysWMEvent
    type : EventType
    timestamp : UInt32
    msg : SysWMmsg*
  end

  union Event
    type : EventType
    common : CommonEvent
    window : WindowEvent
    key : KeyboardEvent
    edit : TextEditingEvent
    text : TextInputEvent
    motion : MouseMotionEvent
    button : MouseButtonEvent
    wheel : MouseWheelEvent
    jaxis : JoyAxisEvent
    jball : JoyBallEvent
    jhat : JoyHatEvent
    jbutton : JoyButtonEvent
    jdevice : JoyDeviceEvent
    caxis : ControllerAxisEvent
    cbutton : ControllerButtonEvent
    cdevice : ControllerDeviceEvent
    quit : QuitEvent
    user : UserEvent
    syswm : SysWMEvent
    tfinger : TouchFingerEvent
    mgesture : MultiGestureEvent
    dgesture : DollarGestureEvent
    drop : DropEvent
    padding : UInt8[56]
  end

  enum EventAction : UInt32
    ADD  = 0
    PEEK = 1
    GET  = 2
  end

  fun pump_events = SDL_PumpEvents()
  fun peep_events = SDL_PeepEvents(events : Event*, numevents : Int, action : EventAction, minType : EventType, maxType : EventType) : Int
  fun has_event = SDL_HasEvent(type : EventType) : Bool
  fun has_events = SDL_HasEvents(minType : EventType, maxType : EventType) : Bool
  fun flush_event = SDL_FlushEvent(type : EventType)
  fun flush_events = SDL_FlushEvents(minType : EventType, maxType : EventType)
  fun poll_event = SDL_PollEvent(event : Event*) : Int
  fun wait_event = SDL_WaitEvent(event : Event*) : Int
  fun wait_event_timeout = SDL_WaitEventTimeout(event : Event*, timeout : Int) : Int
  fun push_event = SDL_PushEvent(event : Event*) : Int

  alias EventFilter = Event* -> Int
  fun set_event_filter = SDL_SetEventFilter(filter : EventFilter*, userdata : Void*)
  fun get_event_filter = SDL_GetEventFilter(filter : EventFilter*, userdata : Void**) : Bool
  fun add_event_watch = SDL_AddEventWatch(filter : EventFilter*, userdata : Void*)
  fun del_event_watch = SDL_DelEventWatch(filter : EventFilter*, userdata : Void*)
  fun filter_events = SDL_FilterEvents(filter : EventFilter*, userdata : Void*)

  QUERY   = -1
  IGNORE  = 0
  DISABLE = 0
  ENABLE  = 1
  fun event_state = SDL_EventState(type : EventType, state : Int) : UInt8
  # SDL_GetEventState(type) = SDL_EventState(type, QUERY)

  fun register_events = SDL_RegisterEvents(numevents : Int) : UInt32
end
