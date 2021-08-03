module SDL
  struct PixelFormat
    delegate format,
      r_mask, g_mask, b_mask, a_mask,
      r_loss, g_loss, b_loss, a_loss,
      r_shift, g_shift, b_shift, a_shift,
      to: @pixel_format.value

    def initialize(@pixel_format : LibSDL::PixelFormat*)
    end

    def bits_per_pixel
      @pixel_format.value.bitsPerPixel
    end

    def bytes_per_pixel
      @pixel_format.value.bitsPerPixel
    end

    #def palette
    #  Palette.new(@pixel_format.value.palette)
    #end

    def to_unsafe
      @pixel_format
    end

    def inspect(io : IO)
      @pixel_format.value.inspect(io)
    end
  end
end
