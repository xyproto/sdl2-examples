require "./rect"
require "./video"
require "./blend_mode"

lib LibSDL
  @[Flags]
  enum RendererFlags : UInt32
    SOFTWARE = 0x00000001
    ACCELERATED = 0x00000002
    PRESENTVSYNC = 0x00000004
    TARGETTEXTURE = 0x00000008
  end

  struct RendererInfo
    name : Char*
    flags : RendererFlags
    num_texture_formats : UInt32
    texture_formats : UInt32[16]
    max_texture_width : Int
    max_texture_height : Int
  end

  enum TextureAccess
    STATIC
    STREAMING
    TARGET
  end

  enum TextureModulate : UInt32
    NONE = 0x00000000
    COLOR = 0x00000001
    ALPHA = 0x00000002
  end

  enum RendererFlip : UInt32
    NONE = 0x00000000
    HORIZONTAL = 0x00000001
    VERTICAL = 0x00000002
  end

  type Renderer = Void
  type Texture = Void

  fun get_num_render_drivers = SDL_GetNumRenderDrivers() : Int
  fun get_render_driver_info = SDL_GetRenderDriverInfo(index : Int, info : RendererInfo*) : Int
  fun create_window_and_renderer = SDL_CreateWindowAndRenderer(width : Int, height : Int, window_flags : UInt32, window : Window**, renderer : Renderer**) : Int
  fun create_renderer = SDL_CreateRenderer(window : Window*, index : Int, flags : UInt32) : Renderer*
  fun create_software_renderer = SDL_CreateSoftwareRenderer(surface : Surface*) : Renderer*
  fun get_renderer = SDL_GetRenderer(window : Window*) : Renderer*
  fun get_renderer_info = SDL_GetRendererInfo(renderer : Renderer*, info : RendererInfo*) : Int
  fun get_renderer_output_size = SDL_GetRendererOutputSize(renderer : Renderer*, w : Int*, h : Int*) : Int
  fun create_texture = SDL_CreateTexture(renderer : Renderer*, format : UInt32, access : Int, w : Int, h : Int) : Texture*
  fun create_texture_from_surface = SDL_CreateTextureFromSurface(renderer : Renderer*, surface : Surface*) : Texture*
  fun query_texture = SDL_QueryTexture(texture : Texture*, format : UInt32*, access : Int*, w : Int*, h : Int*) : Int
  fun set_texture_color_mod = SDL_SetTextureColorMod(texture : Texture*, r : UInt8, g : UInt8, b : UInt8) : Int
  fun get_texture_color_mod = SDL_GetTextureColorMod(texture : Texture*, r : UInt8*, g : UInt8*, b : UInt8*) : Int
  fun set_texture_alpha_mod = SDL_SetTextureAlphaMod(texture : Texture*, alpha : UInt8) : Int
  fun get_texture_alpha_mod = SDL_GetTextureAlphaMod(texture : Texture*, alpha : UInt8*) : Int
  fun set_texture_blend_mode = SDL_SetTextureBlendMode(texture : Texture*, blendMode : BlendMode) : Int
  fun get_texture_blend_mode = SDL_GetTextureBlendMode(texture : Texture*, blendMode : BlendMode*) : Int
  fun update_texture = SDL_UpdateTexture(texture : Texture*, rect : Rect*, pixels : Void*, pitch : Int) : Int
  fun update_yuv_texture = SDL_UpdateYUVTexture(texture : Texture*, rect : Rect*, y_plane : UInt8*, y_pitch : Int, u_plane : UInt8*, u_pitch : Int, v_plane : UInt8*, v_pitch : Int) : Int
  fun lock_texture = SDL_LockTexture(texture : Texture*, rect : Rect*, pixels : Void**, pitch : Int*) : Int
  fun unlock_texture = SDL_UnlockTexture(texture : Texture*)
  fun render_target_supported = SDL_RenderTargetSupported(renderer : Renderer*) : Bool
  fun set_render_target = SDL_SetRenderTarget(renderer : Renderer*, texture : Texture*) : Int
  fun get_render_target = SDL_GetRenderTarget(renderer : Renderer*) : Texture*
  fun render_set_logical_size = SDL_RenderSetLogicalSize(renderer : Renderer*, w : Int, h : Int) : Int
  fun render_get_logical_size = SDL_RenderGetLogicalSize(renderer : Renderer*, w : Int*, h : Int*)
  fun render_set_viewport = SDL_RenderSetViewport(renderer : Renderer*, rect : Rect*) : Int
  fun render_get_viewport = SDL_RenderGetViewport(renderer : Renderer*, rect : Rect*)
  fun render_set_clip_rect = SDL_RenderSetClipRect(renderer : Renderer*, rect : Rect*) : Int
  fun render_get_clip_rect = SDL_RenderGetClipRect(renderer : Renderer*, rect : Rect*)
  fun render_set_scale = SDL_RenderSetScale(renderer : Renderer*, scaleX : Float, scaleY : Float) : Int
  fun render_get_scale = SDL_RenderGetScale(renderer : Renderer*, scaleX : Float*, scaleY : Float*)
  fun set_render_draw_color = SDL_SetRenderDrawColor(renderer : Renderer*, r : UInt8, g : UInt8, b : UInt8, a : UInt8) : Int
  fun get_render_draw_color = SDL_GetRenderDrawColor(renderer : Renderer*, r : UInt8*, g : UInt8*, b : UInt8*, a : UInt8*) : Int
  fun set_render_draw_blend_mode = SDL_SetRenderDrawBlendMode(renderer : Renderer*, blendMode : BlendMode) : Int
  fun get_render_draw_blend_mode = SDL_GetRenderDrawBlendMode(renderer : Renderer*, blendMode : BlendMode*) : Int
  fun render_clear = SDL_RenderClear(renderer : Renderer*) : Int
  fun render_draw_point = SDL_RenderDrawPoint(renderer : Renderer*, x : Int, y : Int) : Int
  fun render_draw_points = SDL_RenderDrawPoints(renderer : Renderer*, points : Point*, count : Int) : Int
  fun render_draw_line = SDL_RenderDrawLine(renderer : Renderer*, x1 : Int, y1 : Int, x2 : Int, y2 : Int) : Int
  fun render_draw_lines = SDL_RenderDrawLines(renderer : Renderer*, points : Point*, count : Int): Int
  fun render_draw_rect = SDL_RenderDrawRect(renderer : Renderer*, rect : Rect*) : Int
  fun render_draw_rects = SDL_RenderDrawRects(renderer : Renderer*, rects : Rect*, count : Int) : Int
  fun render_fill_rect = SDL_RenderFillRect(renderer : Renderer*, rect : Rect*) : Int
  fun render_fill_rects = SDL_RenderFillRects(renderer : Renderer*, rects : Rect*, count : Int) : Int
  fun render_copy = SDL_RenderCopy(renderer : Renderer*, texture : Texture*, srcrect : Rect*, dstrect : Rect*) : Int
  fun render_copy_ex = SDL_RenderCopyEx(renderer : Renderer*, texture : Texture*, srcrect : Rect*, dstrect : Rect*, angle : Double, center : Point*, flip : RendererFlip) : Int
  fun render_read_pixels = SDL_RenderReadPixels(renderer : Renderer*, rect : Rect*, format : UInt32, pixels : Void*, pitch : Int) : Int
  fun render_present = SDL_RenderPresent(renderer : Renderer*)
  fun destroy_texture = SDL_DestroyTexture(texture : Texture*)
  fun destroy_renderer = SDL_DestroyRenderer(renderer : Renderer*)

  fun gl_bind_texture = SDL_GL_BindTexture(texture : Texture*, texw : Float*, texh : Float*) : Int
  fun gl_unbind_texture = SDL_GL_UnbindTexture(texture : Texture*) : Int
end
