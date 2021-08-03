lib LibSDL
  HAT_CENTERED  = 0x00
  HAT_UP        = 0x01
  HAT_RIGHT     = 0x02
  HAT_DOWN      = 0x04
  HAT_LEFT      = 0x08
  HAT_RIGHTUP   = HAT_RIGHT | HAT_UP
  HAT_RIGHTDOWN = HAT_RIGHT | HAT_DOWN
  HAT_LEFTUP    = HAT_LEFT | HAT_UP
  HAT_LEFTDOWN  = HAT_LEFT | HAT_DOWN

  type Joystick = Void
  alias JoystickID = Int32

  struct JoystickGUID
    data : UInt8[16]
  end

  fun num_joysticks = SDL_NumJoysticks() : Int
  fun joystick_name_for_index = SDL_JoystickNameForIndex(device_index : Int) : Char*
  fun joystick_open = SDL_JoystickOpen(device_index : Int) : Joystick*
  fun joystick_name = SDL_JoystickName(joystick : Joystick*) : Char*
  fun joystick_get_device_guid = SDL_JoystickGetDeviceGUID(device_index : Int) : JoystickGUID
  fun joystick_get_guid = SDL_JoystickGetGUID(joystick : Joystick*) : JoystickGUID
  fun joystick_get_guid_string = SDL_JoystickGetGUIDString(guid : JoystickGUID, pszGUID : Char*, cbGUID : Int)
  fun joystick_get_guid_rom_string = SDL_JoystickGetGUIDFromString(pchGuid : Char*)
  fun joystick_get_attached = SDL_JoystickGetAttached(joystick : Joystick*) : Bool
  fun joystick_instance_id = SDL_JoystickInstanceID(joystick : Joystick*) : JoystickID
  fun joystick_num_axes = SDL_JoystickNumAxes(joystick : Joystick*) : Int
  fun joystick_num_balls = SDL_JoystickNumBalls(joystick : Joystick*) : Int
  fun joystick_num_hats = SDL_JoystickNumHats(joystick : Joystick*) : Int
  fun joystick_num_buttons = SDL_JoystickNumButtons(joystick : Joystick*) : Int
  fun joystick_update = SDL_JoystickUpdate()
  fun joystick_event_state = SDL_JoystickEventState(state : Int) : Int
  fun joystick_get_axis = SDL_JoystickGetAxis(joystick : Joystick*, axis : Int) : Int16
  fun joystick_get_hat = SDL_JoystickGetHat(joystick : Joystick*, hat : Int) : UInt8
  fun joystick_get_ball = SDL_JoystickGetBall(joystick : Joystick*, ball : Int, dx : Int*, dy : Int*) : Int
  fun joystick_get_button = SDL_JoystickGetButton(joystick : Joystick*, button : Int) : UInt8
  fun joystick_close = SDL_JoystickClose(joystick : Joystick*)
end
