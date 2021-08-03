lib LibSDL
  SWSURFACE = 0
  PREALLOC  = 0x00000001
  RLEACCEL  = 0x00000002
  DONTFREE  = 0x00000004

  # SDL_MUSTLOCK(S) (((S)->flags & SDL_RLEACCEL) != 0)

  struct Surface
    flags : UInt32
    format : PixelFormat*
    w : Int
    h : Int
    pitch : Int
    pixels : Void*
    userdata : Void*
    locked : Int
    lock_data : Void*
    clip_rect : Rect
    map : Void* # BlitMap*
    refcount : Int
  end

  # alias Blit = (Surface*, Rect*, Surface*, Rect*) -> Int

  fun create_rgb_surface = SDL_CreateRGBSurface(flags : UInt32, width : Int, height : Int, depth : Int, r_mask : UInt32, g_mask : UInt32, b_mask : UInt32, a_mask : UInt32) : Surface*
  fun create_rgb_surface_from = SDL_CreateRGBSurfaceFrom(pixels : Void*, width : Int, height : Int, depth : Int, pitch : Int, r_mask : UInt32, g_mask : UInt32, b_mask : UInt32, a_mask : UInt32) : Surface*
  fun free_surface = SDL_FreeSurface(surface : Surface*)
  fun set_surface_palette = SDL_SetSurfacePalette(surface : Surface*, palette : Palette*) : Int
  fun lock_surface = SDL_LockSurface(surface : Surface*) : Int
  fun unlock_surface = SDL_UnlockSurface(surface : Surface*)

  fun load_bmp_rw = SDL_LoadBMP_RW(src : RWops*, freesrc : Int) : Surface*
  fun save_bmp_rw = SDL_SaveBMP_RW (surface : Surface*, dst : RWops*, freedst : Int) : Int

  fun set_color_key = SDL_SetColorKey(surface : Surface*, flag : Int, key : UInt32) : Int
  fun get_color_key = SDL_GetColorKey(surface : Surface*, key : UInt32*) : Int
  fun set_surface_color_mod = SDL_SetSurfaceColorMod(surface : Surface*, r : UInt8, g : UInt8, b : UInt8) : Int
  fun get_surface_color_mod = SDL_GetSurfaceColorMod(surface : Surface*, r : UInt8*, g : UInt8*, b : UInt8*) : Int
  fun set_surface_alpha_mod = SDL_SetSurfaceAlphaMod(surface : Surface*, alpha : UInt8) : Int
  fun get_surface_alpha_mod = SDL_GetSurfaceAlphaMod(surface : Surface*, alpha : UInt8) : Int
  fun set_surface_blend_mode = SDL_SetSurfaceBlendMode(surface : Surface*, blendMode : BlendMode) : Int
  fun get_surface_blend_mode = SDL_GetSurfaceBlendMode(surface : Surface*, blendMode : BlendMode) : Int
  fun set_clip_rect = SDL_SetClipRect(surface : Surface*, rect : Rect*) : Bool
  fun get_clip_rect = SDL_GetClipRect(surface : Surface*, rect : Rect*) : Bool

  fun convert_surface = SDL_ConvertSurface(src : Surface*, fmt : PixelFormat*, flags : UInt32) : Surface*
  fun convert_surface_format = SDL_ConvertSurfaceFormat(src : Surface*, pixel_format : UInt32, flags : UInt32) : Surface*
  fun convert_pixels = SDL_ConvertPixels(width : Int, height : Int, src_format : UInt32, src : Void*, src_pitch : Int, dst_format : UInt32, dst : Void*, dst_pitch : Int) : Int

  fun fill_rect = SDL_FillRect(dst : Surface*, rect : Rect*, color : UInt32) : Int
  fun fill_rects = SDL_FillRects(dst : Surface*, rects : Rect*, count : Int, color : UInt32) : Int

  #alias blit_surface = SDL_UpperBlit
  fun upper_blit = SDL_UpperBlit(src : Surface*, srcrect : Rect*, dst : Surface*, dstrect : Rect*) : Int
  fun lower_blit = SDL_LowerBlit(src : Surface*, srcrect : Rect*, dst : Surface*, dstrect : Rect*) : Int
  fun soft_stretch = SDL_SoftStretch(src : Surface*, srcrect : Rect*, dst : Surface*, dstrect : Rect*) : Int

  #alias blit_scaled = SDL_UpperBlitScaled
  fun upper_blit_scaled = SDL_UpperBlitScaled(src : Surface*, srcrect : Rect*, dst : Surface*, dstrect : Rect*) : Int
  fun lower_blit_scaled = SDL_LowerBlitScaled(src : Surface*, srcrect : Rect*, dst : Surface*, dstrect : Rect*) : Int
end
