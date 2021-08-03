require "./surface"

module SDL
  class Window
    alias Flags = LibSDL::WindowFlags
    alias Position = LibSDL::WindowPosition

    getter width : Int32
    getter height : Int32

    def initialize(title, @width, @height,
                   x : Position = Position::UNDEFINED,
                   y : Position = Position::UNDEFINED,
                   flags : Flags = Flags::SHOWN)
      @window = LibSDL.create_window(title, x, y, @width, @height, flags)
    end

    def finalize
      LibSDL.destroy_window(self)
    end

    def surface
      @surface ||= begin
        surface = LibSDL.get_window_surface(self)
        raise Error.new("SDL_GetWindowSurface") unless surface
        Surface.new(surface)
      end
    end

    def flags
      LibSDL.get_window_flags(self)
    end

    def title=(title)
      LibSDL.set_window_title(self, title)
    end

    def title
      String.new(LibSDL.get_window_title(self))
    end

    def icon=(icon : Surface)
      LibSDL.set_window_icon(self, icon)
    end

    # Sets the icon from the image located at *path*. Requires `SDL_image`.
    def icon=(path : String)
      self.icon = IMG.load(path)
    end

    {% for name in %w(position size minimum_size maximum_size) %}
      def {{name.id}}=(wh)
        LibSDL.set_window_{{name.id}}(self, *wh)
        wh
      end

      def {{name.id}}
        LibSDL.get_window_{{name.id}}(self, out w, out h)
        {w, h}
      end
    {% end %}

    def bordered=(value)
      LibSDL.set_window_bordered(self, value ? 1 : 0)
    end

    {% for name in %w(show hide raise maximize minimize restore) %}
      def {{name.id}}
        LibSDL.{{name.id}}_window(self)
      end
    {% end %}

    enum Fullscreen
      WINDOW = 0
      FULLSCREEN = LibSDL::WindowFlags::FULLSCREEN
      FULLSCREEN_DESKTOP = LibSDL::WindowFlags::FULLSCREEN_DESKTOP
    end

    def fullscreen=(flag : Fullscreen)
      ret = LibSDL.set_window_fullscreen(self, flag)
      raise Error.new("SDL_SetWindowFullscreen") unless ret == 0
      flag
    end

    def fullscreen=(value : Bool)
      self.fullscreen = value ? Fullscreen::FULLSCREEN_DESKTOP : Fullscreen::WINDOW
    end

    def grab=(value)
      LibSDL.set_window_grab(self, value ? 1 : 0)
    end

    def grab
      LibSDL.get_window_grab(self) == 1
    end

    def brightness=(value)
      ret = LibSDL.set_window_brightness(self, value)
      raise Error.new("SDL_SetWindowBrightness") unless ret == 0
      value
    end

    def brightness
      LibSDL.get_window_brightness(self)
    end

    def gamma_ramp=(rgb)
      ret = LibSDL.set_window_gamma_ramp(self, *rgb)
      raise Error.new("SDL_SetWindowGammaRamp") unless ret == 0
      rgb
    end

    def gamma_ramp
      ret = LibSDL.get_window_gamma_ramp(self, out r, out g, out b)
      raise Error.new("SDL_GetWindowGammaRamp") unless ret == 0
      {r, g, b}
    end

    # Copies the window `surface` to the screen.
    def update
      LibSDL.update_window_surface(self)
    end

    def to_unsafe
      @window
    end
  end
end
