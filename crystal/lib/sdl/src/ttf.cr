require "./lib_ttf"
require "./sdl"
require "./color"

module SDL
  module TTF
    alias Error = SDL::Error

    def self.init
      if LibTTF.was_init == 1
        return
      end
      unless LibTTF.init == 0
        raise SDL::Error.new("TTF_Init failed to init")
      end
    end

    def self.quit
      LibTTF.quit if LibTTF.was_init == 1
    end

    class Font
      alias Style = LibTTF::Style

      enum Mode
        SOLID
        SHADED
        BLENDED
        BLENDED_WRAPPED
      end

      def self.open(path)
        font = new(path)
        begin
          yield font
        ensure
          font.close
        end
      end

      def initialize(path, point_size = 16, index = nil)
        @rwops = SDL::RWops.new(path, "rb")
        if index
          @font = LibTTF.open_font_index_rw(@rwops, 1, point_size, index)
        else
          @font = LibTTF.open_font_rw(@rwops, 1, point_size)
        end
        @closed = false
      end

      def finalize
        close unless @closed
      end

      def kerning=(allowed : Bool)
        LibTTF.set_font_kerning(self, allowed ? 1 : 0)
      end

      def style
        Style.new(LibTTF.get_font_style(self))
      end

      def style=(style : Style)
        LibTTF.set_font_style(self, style)
      end

      def outline
        LibTTF.get_font_outline(self)
      end

      def outline=(outline)
        LibTTF.set_font_outline(self, outline)
      end

      # Returns the maximum pixel height of all glyphs of the font.
      def height
        LibTTF.font_height(self)
      end

      # Returns the maximum pixel ascent of all glyphs of the font.
      def ascent
        LibTTF.font_ascent(self)
      end

      # Returns the maximum pixel descent of all glyphs of the font.
      def descent
        LibTTF.font_descent(self)
      end

      # Returns the reccomended pixel height of a rendered line of text of the
      # font.
      def line_skip
        LibTTF.font_line_skip(self)
      end

      # Returns the number of faces (sub-fonts) available in the font.
      def faces
        LibTTF.font_faces(self)
      end

      def face_fixed_width?
        LibTTF.font_face_is_fixed_width(self) == 1
      end

      def face_family_name
        if chars = LibTTF.font_face_family_name(self)
          String.new(chars)
        end
      end

      def face_style_name
        if chars = LibTTF.font_face_style_name(self)
          String.new(chars)
        end
      end

      def glyph_provided?(char : UInt16)
        LibTTF.glyph_is_provided(self, char) == 1
      end

      record GlyphMetrics,
        char : UInt16,
        min_x : Int32,
        min_y : Int32,
        max_x : Int32,
        max_y : Int32,
        advance : Int32

      def glyph_metrics(char : UInt16)
        unless glyph_provided?(char)
          return
        end
        if LibTTF.glyph_metrics(self, char, out minx, out maxx, out miny, out maxy, out advance) == 0
          Metrics.new(char, minx, miny, maxx, maxy, advance)
        else
          raise Error.new("TTF_GlyphMetrics")
        end
      end

      # Returns the `{width, height}` of *text* if rendered with the current font.
      def size_of(text : String, ascii = false)
        if ascii
          ret = LibTTF.size_ascii(self, text, out width, out height)
          raise Error.new("TTF_SizeText") unless ret == 0
        else
          ret = LibTTF.size_utf8(self, text, out width, out height)
          raise Error.new("TTF_SizeUTF8") unless ret == 0
        end
        {width, height}
      end

      # Returns the `width` of *text* if rendered with the current font.
      def width_of(text : String, ascii = false)
        if ascii
          ret = LibTTF.size_ascii(self, text, out width, nil)
          raise Error.new("TTF_SizeText") unless ret == 0
        else
          ret = LibTTF.size_utf8(self, text, out width, nil)
          raise Error.new("TTF_SizeUTF8") unless ret == 0
        end
        width
      end

      # Returns the `height` of *text* if rendered with the current font.
      def height_of(text : String, ascii = false)
        if ascii
          ret = LibTTF.size_ascii(self, text, nil, out height)
          raise Error.new("TTF_SizeText") unless ret == 0
        else
          ret = LibTTF.size_utf8(self, text, nil, out height)
          raise Error.new("TTF_SizeUTF8") unless ret == 0
        end
        height
      end

      # Renders text using this font as an `SDL::Surface` using the "slow and
      # nice" shaded mode.
      def render(text : String, color, background, ascii = false)
        render_shaded(text, color, background, ascii)
      end

      # Renders text using this font as an `SDL::Surface` using the "quick and
      # dirty" solid mode.
      def render_solid(text : String, color, ascii = false)
        if ascii
          surface = LibTTF.render_text_solid(self, text, color)
          raise Error.new("TTF_RenderText_Solid") unless surface
        else
          surface = LibTTF.render_utf8_solid(self, text, color)
          raise Error.new("TTF_RenderUTF8_Solid") unless surface
        end
        SDL::Surface.new(surface)
      end

      # Renders text using this font as an `SDL::Surface` using the "slow and
      # nice" shaded mode.
      def render_shaded(text : String, color, background, ascii = false)
        if ascii
          surface = LibTTF.render_text_shaded(self, text, color, background)
          raise Error.new("TTF_RenderText_Shaded") unless surface
        else
          surface = LibTTF.render_utf8_shaded(self, text, color, background)
          raise Error.new("TTF_RenderUTF8_Shaded") unless surface
        end
        SDL::Surface.new(surface)
      end

      # Renders text using this font as an `SDL::Surface` using the "slow, slow,
      # slow but ultra nice over another image" blended mode.
      def render_blended(text : String, color, ascii = false)
        if ascii
          surface = LibTTF.render_text_blended(self, text, color)
          raise Error.new("TTF_RenderText_Blended") unless surface
        else
          surface = LibTTF.render_utf8_blended(self, text, color)
          raise Error.new("TTF_RenderUTF8_Blended") unless surface
        end
        SDL::Surface.new(surface)
      end

      # Renders text using this font as an `SDL::Surface` using the blended
      # wrapped mode.
      #def render_blended_wrapper(text : String, color, wrap_length, ascii = false)
      #  if ascii
      #    surface = LibTTF.render_text_blended_wrapped(self, text, color, wrap_length)
      #    raise Error.new("TTF_RenderText_Blended_Wrapped") unless surface
      #  else
      #    surface = LibTTF.render_utf8_blended_wrapped(self, text, color, wrap_length)
      #    raise Error.new("TTF_RenderUTF8_Blended_Wrapped") unless surface
      #  end
      #  SDL::Surface.new(surface)
      #end

      def close
        @closed = true
        LibTTF.close_font(self)
        @rwops.close
      end

      def to_unsafe
        @font
      end
    end
  end
end
