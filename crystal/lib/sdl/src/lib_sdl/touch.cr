lib LibSDL
  alias TouchID = Int64
  alias FingerID = Int64

  struct Finger
    id : FingerID
    x : Float
    y : Float
    pressure : Float
  end

  fun get_num_touch_devices = SDL_GetNumTouchDevices() : Int
  fun get_touch_device = SDL_GetTouchDevice(index : Int) : TouchID
  fun get_num_touch_fingers = SDL_GetNumTouchFingers(touchID : TouchID) : Int
  fun get_touch_finger = SDL_GetTouchFinger(touchID : TouchID, index : Int) : Finger*
end
