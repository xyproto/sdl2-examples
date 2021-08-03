require "./lib_sdl"

@[Link("SDL2_ttf")]
lib LibTTF
  alias Char = LibC::Char
  alias Int = LibC::Int
  alias Long = LibC::Long

  VERSION = {% `pkg-config SDL2_ttf --modversion`.strip %}
  MAJOR = {% VERSION.split('.')[0] %}
  MINOR = {% VERSION.split('.')[1] %}
  PATCH = {% VERSION.split('.')[2] %}

  enum Style
    NORMAL        = 0x00
    BOLD          = 0x01
    ITALIC        = 0x02
    UNDERLINE     = 0x04
    STRIKETHROUGH = 0x08
  end

  type Font = Void

  #fun linked_version = TTF_Linked_Version() : LibSDL::Version
  fun init = TTF_Init() : Int
  fun was_init = TTF_WasInit() : Int
  fun quit = TTF_Quit()

  fun open_font = TTF_OpenFont(file : Char*, ptsize : Int) : Font*
  fun open_font_index = TTF_OpenFontIndex(file : Char*, ptsize : Int, index : Long) : Font*
  fun open_font_rw = TTF_OpenFontRW(src : LibSDL::RWops*, freesrc : Int, ptsize : Int) : Font*
  fun open_font_index_rw = TTF_OpenFontIndexRW(src : LibSDL::RWops*, freesrc : Int, ptsize : Int, index : Long) : Font*
  fun close_font = TTF_CloseFont(font : Font*)

  fun byte_swap_unicode = TTF_ByteSwappedUNICODE(swapped : Int)
  fun get_font_style = TTF_GetFontStyle(font : Font*) : Int
  fun set_font_style = TTF_SetFontStyle(font : Font*, style : Style)
  fun get_font_outline = TTF_GetFontOutline(font : Font*) : Int
  fun set_font_outline = TTF_SetFontOutline(font : Font*, outline : Int)
  fun font_height = TTF_FontHeight(font : Font*) : Int
  fun font_ascent = TTF_FontAscent(font : Font*) : Int
  fun font_descent = TTF_FontDescent(font : Font*) : Int
  fun font_line_skip = TTF_FontLineSkip(font : Font*) : Int
  fun set_font_kerning = TTF_SetFontKerning(font : Font*, allowed : Int)
  fun font_faces = TTF_FontFaces(font : Font*)
  fun font_face_is_fixed_width = TTF_FontFaceIsFixedWidth(font : Font*) : Int
  fun font_face_family_name = TTF_FontFaceFamilyName(font : Font*) : Char*
  fun font_face_style_name = TTF_FontFaceStyleName(font : Font*) : Char*
  fun glyph_is_provided = TTF_GlyphIsProvided(font : Font*, ch : UInt16) : Int
  fun glyph_metrics = TTF_GlyphMetrics(font : Font*, ch : UInt16, minx : Int*, maxx : Int*, miny : Int*, maxy : Int*, advance : Int*) : Int
  fun size_text = TTF_SizeText(font : Font*, text : Char*, w : Int*, h : Int*) : Int
  fun size_utf8 = TTF_SizeUTF8(font : Font*, text : Char*, w : Int*, h : Int*) : Int
  fun size_unicode = TTF_SizeUNICODE(font : Font*, text : UInt16*, w : Int*, h : Int*) : Int

  fun render_text_solid = TTF_RenderText_Solid(font : Font*, text : Char*, fg : LibSDL::Color) : LibSDL::Surface*
  fun render_utf8_solid = TTF_RenderUTF8_Solid(font : Font*, text : Char*, fg : LibSDL::Color) : LibSDL::Surface*
  fun render_unicode_solid = TTF_RenderUNICODE_Solid(font : Font*, text : UInt16*, fg : LibSDL::Color) : LibSDL::Surface*
  fun render_glyph_solid = TTF_RenderGlyph_Solid(font : Font*, ch : UInt16, fg : LibSDL::Color): LibSDL::Surface*

  fun render_text_shaded = TTF_RenderText_Shaded(font : Font*, text : Char*, fg : LibSDL::Color, bg : LibSDL::Color) : LibSDL::Surface*
  fun render_utf8_shaded = TTF_RenderUTF8_Shaded(font : Font*, text : Char*, fg : LibSDL::Color, bg : LibSDL::Color) : LibSDL::Surface*
  fun render_unicode_shaded = TTF_RenderUNICODE_Shaded(font : Font*, text : UInt16*, fg : LibSDL::Color, bg : LibSDL::Color) : LibSDL::Surface*
  fun render_glyph_shaded = TTF_RenderGlyph_Shaded(font : Font*, ch : UInt16, fg : LibSDL::Color, bg : LibSDL::Color) : LibSDL::Surface*

  fun render_text_blended  = TTF_RenderText_Blended(font : Font*, text : Char*, fg : LibSDL::Color) : LibSDL::Surface*
  fun render_utf8_blended  = TTF_RenderUTF8_Blended(font : Font*, text : Char*, fg : LibSDL::Color) : LibSDL::Surface*
  fun render_unicode_blended = TTF_RenderUNICODE_Blended(font : Font*, text : UInt16*, fg : LibSDL::Color) : LibSDL::Surface*
  fun render_glyph_blended = TTF_RenderGlyph_Blended(font : Font*, ch : UInt16, fg : LibSDL::Color) : LibSDL::Surface*

  fun render_text_blended_wrapped = TTF_RenderText_Blended_Wrapped(font : Font*, text : Char*, fg : LibSDL::Color, wrap_length : UInt32) : LibSDL::Surface*
  fun render_utf8_blended_wrapped = TTF_RenderUTF8_Blended_Wrapped(font : Font*, text : Char*, fg : LibSDL::Color, wrap_length : UInt32) : LibSDL::Surface*
  fun render_unicode_blended_wrapped = TTF_RenderUNICODE_Blended_Wrapped(font : Font*, text : UInt16*, fg : LibSDL::Color, wrap_length : UInt32) : LibSDL::Surface*
end
