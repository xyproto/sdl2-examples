module SDL
  abstract struct Event
    alias Type = LibSDL::EventType

    module PressRelease
      def pressed?
        _event.state == LibSDL::PRESSED
      end

      def released?
        _event.state == LibSDL::RELEASED
      end
    end

    struct Window < Event
      protected def _event
        @event.window
      end

      delegate event, data1, data2, to: _event

      def window_id
        _event.windowID
      end
    end

    struct Keyboard < Event
      include PressRelease

      protected def _event
        @event.key
      end

      delegate repeat, keysym, to: _event

      def window_id
        _event.windowID
      end

      def keyup?
        type.keyup?
      end

      def keydown?
        type.keydown?
      end

      def mod
        _event.keysym.mod
      end

      def sym
        _event.keysym.sym
      end
    end

    struct TextEditing < Event
      protected def _event
        @event.edit
      end

      delegate text, start, length, to: _event

      def window_id
        _event.windowID
      end
    end

    struct TextInput < Event
      protected def _event
        @event.text
      end

      delegate text, to: _event

      def window_id
        _event.windowID
      end
    end

    struct MouseMotion < Event
      include PressRelease

      protected def _event
        @event.motion
      end

      delegate which, x, y, xrel, yrel, to: _event

      def window_id
        _event.windowID
      end
    end

    struct MouseButton < Event
      include PressRelease

      protected def _event
        @event.button
      end

      delegate which, button, clicks, x, y, to: _event

      def window_id
        _event.windowID
      end
    end

    struct MouseWheel < Event
      protected def _event
        @event.wheel
      end

      delegate which, x, y, to: _event

      def window_id
        _event.windowID
      end
    end

    struct JoyAxis < Event
      protected def _event
        @event.jaxis
      end

      delegate which, x, y, to: _event
    end

    struct JoyBall < Event
      protected def _event
        @event.jball
      end

      delegate which, ball, to: _event

      def xrel
        _event.xrel.to_i
      end

      def yrel
        _event.yrel.to_i
      end
    end

    struct JoyHat < Event
      protected def _event
        @event.jhat
      end

      delegate which, hat, value, to: _event
    end

    struct JoyButton < Event
      include PressRelease

      protected def _event
        @event.jbutton
      end

      delegate which, button, to: _event
    end

    struct JoyDevice < Event
      protected def _event
        @event.jdevice
      end

      delegate which, to: _event
    end

    struct ControllerAxis < Event
      protected def _event
        @event.caxis
      end

      delegate which, axis, value, to: _event
    end

    struct ControllerButton < Event
      include PressRelease

      protected def _event
        @event.cbutton
      end

      delegate which, button, to: _event
    end

    struct ControllerDevice < Event
      protected def _event
        @event.jdevice
      end

      delegate which, to: _event
    end

    struct TouchFinger < Event
      protected def _event
        @event.tfinger
      end

      delegate x, y, dx, dy, pressure, to: _event

      def touch_id
        _event.touchId
      end

      def finger_id
        _event.fingerId
      end
    end

    struct DollarGesture < Event
      protected def _event
        @event.dgesture
      end

      delegate error, x, y, to: _event

      def touch_id
        _event.touchId
      end

      def gesture_id
        _event.gestureId
      end

      def num_fingers
        _event.numFingers
      end
    end

    struct MultiGesture < Event
      protected def _event
        @event.mgesture
      end

      delegate x, y, to: _event

      def touch_id
        _event.touchId
      end

      def d_theta
        _event.dTheta
      end

      def d_dist
        _event.dDist
      end

      def num_fingers
        _event.numFingers
      end
    end

    struct Drop < Event
      protected def _event
        @event.drop
      end

      def filename
        String.new(_event.file)
      end
    end

    struct Quit < Event
      protected def _event
        @event.quit
      end
    end

    struct User < Event
      protected def _event
        @event.user
      end

      delegate code, data1, data2, to: _event

      def window_id
        _event.windowID
      end

      def push
        ret = LibSDL.push_event(self)
        raise Error.new("SDL_PushEvent") unless ret == 0
      end
    end

    struct SysWM < Event
      protected def _event
        @event.syswm
      end

      delegate msg, to: _event
    end

    # Ignores an event type. They will no longer be pushed to the queue event.
    def self.ignore(type : Type) : Nil
      LibSDL.event_state(type, LibSDL::EventState::IGNORE)
    end

    # Returns true if an event type is ignored.
    def self.ignored?(type : Type)
      LibSDL.event_state(type, LibSDL::EventState::QUERY) == LibSDL::EventState::IGNORE
    end

    # Enables an event type. They will be pushed to the event queue.
    def self.enable(type : Type) : Nil
      LibSDL.event_state(type, LibSDL::EventState::ENABLE)
    end

    # Returns true if an event type is enabled.
    def self.enabled?(type : Type) : Nil
      LibSDL.event_state(type, LibSDL::EventState::QUERY) == LibSDL::EventState::ENABLE
    end

    # Tries to pull an event from the event queue; blocks until an event is added
    # to the queue, indefinitely, or for a given timeout.
    def self.wait(timeout = nil)
      event = uninitialized LibSDL::Event
      if timeout
        ret = LibSDL.wait_event_timeout(pointerof(event), timeout)
        raise Error.new("SDL_WaitEventTimeout") unless ret == 1
      else
        ret = LibSDL.wait_event(pointerof(event))
        raise Error.new("SDL_WaitEvent") unless ret == 1
      end
      from(event)
    end

    # Tries to pull an event from the event queue; returns nil immediately if the
    # queue is empty.
    def self.poll
      case LibSDL.poll_event(out event)
      when 1
        from(event)
      when -1
        raise Error.new("SDL_PollEvent")
      end
    end

    protected def self.from(event : LibSDL::Event)
      case event.type
      when .window_event?
        Window.new(event)

      when .keydown?, .keyup?
        Keyboard.new(event)
      when .text_editing?
        TextEditing.new(event)
      when .text_input?
        TextInput.new(event)

      when .mouse_motion?
        MouseMotion.new(event)
      when .mouse_button_down?, .mouse_button_up?
        MouseButton.new(event)
      when .mouse_wheel?
        MouseWheel.new(event)

      when .joy_axis_motion?
        JoyAxis.new(event)
      when .joy_ball_motion?
        JoyBall.new(event)
      when .joy_hat_motion?
        JoyHat.new(event)
      when .joy_button_down?, .joy_button_up?
        JoyButton.new(event)
      when .joy_device_added?, .joy_device_removed?
        JoyDevice.new(event)

      when .controller_axis_motion?
        ControllerAxis.new(event)
      when .controller_button_down?, .controller_button_up?
        ControllerButton.new(event)
      when .controller_device_added?, .controller_device_removed?, .controller_device_remapped?
        ControllerDevice.new(event)

      when .finger_down?, .finger_up?, .finger_motion?
        TouchFinger.new(event)
      when .dollar_gesture?, .dollar_record?
        DollarGesture.new(event)
      when .multi_gesture?
        MultiGesture.new(event)

      when .drop_file?
        Drop.new(event)

      when .quit?
        Quit.new(event)
      when .sys_wm_event?
        SysWM.new(event)
      else
        User.new(event)
      end
    end

    def initialize(@event : LibSDL::Event)
    end

    def type
      _event.type
    end

    def timestamp
      _event.timestamp
    end

    # :nodoc:
    def to_unsafe
      pointerof(@event)
    end
  end
end
