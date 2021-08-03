require "./rect"
require "./pixels"
require "./rwops"

module SDL
  SWSURFACE = LibSDL::SWSURFACE
  PREALLOC  = LibSDL::PREALLOC
  RLEACCEL  = LibSDL::RLEACCEL
  DONTFREE  = LibSDL::DONTFREE

  def self.load_bmp(path)
    RWops.open(path, "rb") do |rwops|
      surface = LibSDL.load_bmp_rw(rwops, 1)
      raise Error.new("SDL_LoadBMP_RW") unless surface
      Surface.new(surface)
    end
  end

  class Surface
    def self.from(buffer : Bytes, width, height, format : PixelFormat)
      surface = LibSDL.create_rgb_surface_from(buffer, width, height, format.bits_per_pixel, width * format.bytes_per_pixel, format.r_mask, format.g_mask, format.b_mask, format.a_mask)
      raise SDL::Error.new("SDL_RenderReadPixels") unless surface
      SDL::Surface.new(surface)
    end

    def initialize(@surface : LibSDL::Surface*)
    end

    def finalize
      LibSDL.free_surface(self)
    end

    def flags
      surface.flags
    end

    def width
      surface.w
    end

    def height
      surface.h
    end

    def pitch
      surface.pitch
    end

    def set_color_key(rgb, flag = 1)
      ret = LibSDL.set_color_key(self, flag, color(*rgb))
      raise Error.new("SDL_SetColorKey") unless ret == 0
    end

    def color_key=(rgb)
      set_color_key(rgb, 1)
    end

    def color_key
      ret = LibSDL.get_color_key(self, out rgb)
      raise Error.new("SDL_GetColorKey") unless ret == 0
      rbg
    end

    def color_mod=(rgb)
      ret = LibSDL.set_surface_color_mod(self, *rgb)
      raise Error.new("SDL_SetSurfaceColorMod") unless ret == 0
      rgb
    end

    def color_mod
      ret = LibSDL.get_surface_color_mod(self, out r, out g, out b)
      raise Error.new("SDL_SetSurfaceColorMod") unless ret == 0
      {r, g, b}
    end

    def alpha_mod=(alpha)
      ret = LibSDL.set_surface_alpha_mod(self, alpha)
      raise Error.new("SDL_SetSurfaceAlphaMod") unless ret == 0
      alpha
    end

    def alpha_mod
      ret = LibSDL.get_surface_alpha_mod(self, out alpha)
      raise Error.new("SDL_SetSurfaceColorMod") unless ret == 0
      alpha
    end

    def blend_mode=(blend_mode : BlendMode)
      ret = LibSDL.set_surface_blend_mode(self, blend_mode)
      raise Error.new("SDL_SetSurfaceBlendMode") unless ret == 0
      blend_mode
    end

    def blend_mode
      ret = LibSDL.get_surface_blend_mode(self, out blend_mode)
      raise Error.new("SDL_GetSurfaceBlendMode") unless ret == 0
      blend_mode
    end

    def clip_rect=(rect)
      ret = LibSDL.set_clip_rect(self, pointerof(rect))
      raise Error.new("SDL_SetClipRect") unless ret == 1
      rect
    end

    def clip_rect
      ret = LibSDL.get_clip_rect(self, out rect)
      raise Error.new("SDL_GetClipRect") unless ret == 1
      rect
    end

    # Maps an RGB(A) color for this surface.
    def color(r, g, b, a = nil)
      if a
        LibSDL.map_rgba(surface.format, r, g, b, a)
      else
        LibSDL.map_rgb(surface.format, r, g, b)
      end
    end

    # Fill the whole surface with a RGB(A) color.
    def fill(r, g, b, a = nil)
      fill(nil, r, g, b, a)
    end

    # Fill the whole surface with a RGB(A) color.
    def fill(rect : Nil, r, g, b, a = nil)
      LibSDL.fill_rect(self, nil, color(r, g, b, a))
    end

    # Fill a *rect* of the surface with a RGB(A) color.
    def fill(rect : SDL::Rect, r, g, b, a = nil)
      LibSDL.fill_rect(self, pointerof(rect), color(r, g, b, a))
    end

    # Saves the Surface as a BMP image.
    def save_bmp(path)
      RWops.open(path, "wb") do |rwops|
        if LibSDL.save_bmp_rw(self, rwops, 1) != 0
          raise Error.new("SDL_SaveBMP_RW")
        end
      end
    end

    # Fast copy of this Surface to *dst* Surface.
    def blit(dst : Surface, srcrect = nil, dstrect = nil)
      if LibSDL.upper_blit(self, SDL.pointer_or_null(srcrect, Rect), dst, SDL.pointer_or_null(dstrect, Rect)) != 0
        raise Error.new("SDL_BlitSurface")
      end
    end

    # Fast scaled copy of this Surface to *dst* Surface. Scales to the whole
    # surface by default.
    def blit_scaled(dst : Surface, srcrect = nil, dstrect = nil)
      if LibSDL.upper_blit_scaled(self, SDL.pointer_or_null(srcrect, Rect), dst, SDL.pointer_or_null(dstrect, Rect)) != 0
        raise Error.new("SDL_BlitScaled")
      end
    end

    # Copy this surface into a new one that is optimized for blitting to
    # the given *surface*.
    def convert(surface)
      optimized = LibSDL.convert_surface(self, surface.format, 0)
      raise Error.new("SDL_ConvertSurface") unless optimized
      Surface.new(optimized)
    end

    def lock
      unless must_lock?
        return yield
      end

      ret = LibSDL.lock_surface(self)
      raise Error.new("SDL_LockSurface") unless ret == 0

      begin
        yield
      ensure
        LibSDL.unlock_surface(self)
      end
    end

    private def must_lock?
      flags & LibSDL::RLEACCEL != 0
    end

    def save_bmp(path)
      RWops.open(path, "wb") do |rwops|
        ret = LibSDL.save_bmp_rw(self, rwops, 1)
        raise Error.new("SDL_LoadBMP_RW") unless ret == 0
      end
    end

    # Requires `SDL_image`.
    #def save_png(path)
    #end

    private def surface
      @surface.value
    end

    def format
      PixelFormat.new(surface.format)
    end

    #protected def pixels
    #  surface.pixels
    #end

    def to_unsafe
      @surface
    end
  end
end
