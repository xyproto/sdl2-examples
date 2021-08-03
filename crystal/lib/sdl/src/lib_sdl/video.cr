require "./pixels"
require "./rect"
require "./surface"

lib LibSDL
  struct DisplayMode
    format : UInt32
    w : Int
    h : Int
    refresh_rate : Int
    driverdata : Void*
  end

  type Window = Void

  @[Flags]
  enum WindowFlags : UInt32
    FULLSCREEN = 0x00000001
    OPENGL = 0x00000002
    SHOWN = 0x00000004
    HIDDEN = 0x00000008
    BORDERLESS = 0x00000010
    RESIZABLE = 0x00000020
    MINIMIZED = 0x00000040
    MAXIMIZED = 0x00000080
    INPUT_GRABBED = 0x00000100
    INPUT_FOCUS = 0x00000200
    MOUSE_FOCUS = 0x00000400
    FULLSCREEN_DESKTOP = 0x00001001 # WINDOW_FULLSCREEN | 0x00001000
    FOREIGN = 0x00000800
    ALLOW_HIGHDPI = 0x00002000
    ALWAYS_ON_TOP = 0x00008000 # x11 only
    SKIP_TASKBAR = 0x00010000 # x11 only
    UTILITY = 0x00020000 # x11 only
    TOOLTIP = 0x00040000 # x11 only
    POPUP_MENU = 0x00080000 # x11 only
  end

  enum WindowPosition
    UNDEFINED = 0x1FFF0000
    CENTERED  = 0x2FFF0000
  end

  enum WindowEventID
    NONE
    SHOWN
    HIDDEN
    EXPOSED
    MOVED
    RESIZED
    SIZE_CHANGED
    MINIMIZED
    MAXIMIZED
    RESTORED
    ENTER
    LEAVE
    FOCUS_GAINED
    FOCUS_LOST
    CLOSE
  end

  type GLContext = Void*

  enum GLattr
    SDL_GL_RED_SIZE
    SDL_GL_GREEN_SIZE
    SDL_GL_BLUE_SIZE
    SDL_GL_ALPHA_SIZE
    SDL_GL_BUFFER_SIZE
    SDL_GL_DOUBLEBUFFER
    SDL_GL_DEPTH_SIZE
    SDL_GL_STENCIL_SIZE
    SDL_GL_ACCUM_RED_SIZE
    SDL_GL_ACCUM_GREEN_SIZE
    SDL_GL_ACCUM_BLUE_SIZE
    SDL_GL_ACCUM_ALPHA_SIZE
    SDL_GL_STEREO
    SDL_GL_MULTISAMPLEBUFFERS
    SDL_GL_MULTISAMPLESAMPLES
    SDL_GL_ACCELERATED_VISUAL
    SDL_GL_RETAINED_BACKING
    SDL_GL_CONTEXT_MAJOR_VERSION
    SDL_GL_CONTEXT_MINOR_VERSION
    SDL_GL_CONTEXT_EGL
    SDL_GL_CONTEXT_FLAGS
    SDL_GL_CONTEXT_PROFILE_MASK
    SDL_GL_SHARE_WITH_CURRENT_CONTEXT
    SDL_GL_FRAMEBUFFER_SRGB_CAPABLE
  end

  enum GLprofile
    PROFILE_CORE           = 0x0001
    PROFILE_COMPATIBILITY  = 0x0002
    PROFILE_ES             = 0x0004
  end

  enum GLcontextFlag
    DEBUG_FLAG              = 0x0001
    FORWARD_COMPATIBLE_FLAG = 0x0002
    ROBUST_ACCESS_FLAG      = 0x0004
    RESET_ISOLATION_FLAG    = 0x0008
  end

  struct SDL_Version
    major : UInt8
    minor : UInt8
    patch : UInt8
  end

  enum SDL_SYSType
    SDL_SYSWM_UNKNOWN
    SDL_SYSWM_WINDOWS
    SDL_SYSWM_X11
    SDL_SYSWM_DIRECTFB
    SDL_SYSWM_COCOA
    SDL_SYSWM_UIKIT
    SDL_SYSWM_WAYLAND
    SDL_SYSWM_MIR
    SDL_SYSWM_WINRT
    SDL_SYSWM_ANDROID
    SDL_SYSWM_VIVANTE
    SDL_SYSWM_OS2
  end

  struct SDL_WMInfoX11
    display : Void*
    window : UInt32
  end

  union SDL_WMInfoUnion
    x11 : SDL_WMInfoX11
  end

  struct SDL_WMInfo
    version : SDL_Version
    subsystem : SDL_SYSType
    info : SDL_WMInfoUnion
  end

  fun get_num_video_drivers = SDL_GetNumVideoDrivers() : Int
  fun get_video_driver = SDL_GetVideoDriver(index : Int) : Char*
  fun video_init = SDL_VideoInit(driver_name : Char*) : Int
  fun video_quit = SDL_VideoQuit()
  fun get_current_video_driver = SDL_GetCurrentVideoDriver() : Char*
  fun get_num_video_displays = SDL_GetNumVideoDisplays() : Int
  fun get_display_name = SDL_GetDisplayName(displayIndex : Int) : Char*
  fun get_display_bounds = SDL_GetDisplayBounds(displayIndex : Int, rect : Rect*) : Int
  fun get_display_usable_bounds = SDL_GetDisplayUsableBounds(displayIndex : Int, rect : Rect*) : Int
  fun get_num_display_modes = SDL_GetNumDisplayModes(displayIndex : Int) : Int
  fun get_display_mode = SDL_GetDisplayMode(displayIndex : Int, modeIndex : Int, mode : DisplayMode*) : Int
  fun get_desktop_display_mode = SDL_GetDesktopDisplayMode(displayIndex : Int, mode : DisplayMode*) : Int
  fun get_current_display_mode = SDL_GetCurrentDisplayMode(displayIndex : Int, mode : DisplayMode*) : Int
  fun get_closest_display_mode = SDL_GetClosestDisplayMode(displayIndex : Int, mode : DisplayMode*, closest : DisplayMode*) : DisplayMode*
  fun get_window_display_index = SDL_GetWindowDisplayIndex(window : Window*) : Int
  fun set_window_display_mode = SDL_SetWindowDisplayMode(window : Window*, mode : DisplayMode*) : Int
  fun get_window_display_mode = SDL_GetWindowDisplayMode(window : Window*, mode : DisplayMode*) : Int
  fun get_window_pixel_mode = SDL_GetWindowPixelFormat(window : Window*) : UInt32

  fun create_window = SDL_CreateWindow(title : Char*, x : Int, y : Int, w : Int, h : Int, flags : UInt32) : Window*
  fun create_window_from = SDL_CreateWindowFrom(data : Void*) : Window*
  fun get_window_id = SDL_GetWindowID(window : Window*) : UInt32
  fun get_window_from_id = SDL_GetWindowFromID(id : UInt32) : Window*
  fun get_window_flags = SDL_GetWindowFlags(window : Window*) : WindowFlags
  fun get_window_wm_info = SDL_GetWindowWMInfo(window : Window*, info : SDL_WMInfo*) : Bool
  fun set_window_title = SDL_SetWindowTitle(window : Window*, title : Char*)
  fun get_window_title = SDL_GetWindowTitle(window : Window*) : Char*
  fun set_window_icon = SDL_SetWindowIcon(window : Window*, icon : Surface*)
  fun set_window_data = SDL_SetWindowData(window : Window*, name : Char*, userdata : Void*) : Void*
  fun get_window_data = SDL_GetWindowData(window : Window*, name : Char*) : Void*
  fun set_window_position = SDL_SetWindowPosition(window : Window*, x : Int, y : Int)
  fun get_window_position = SDL_GetWindowPosition(window : Window*, x : Int*, y : Int*)
  fun set_window_size = SDL_SetWindowSize(window : Window*, w : Int, h : Int)
  fun get_window_size = SDL_GetWindowSize(window : Window*, w : Int*, h : Int*)
  fun set_window_minimum_size = SDL_SetWindowMinimumSize(window : Window*, min_w : Int, min_h : Int)
  fun get_window_minimum_size = SDL_GetWindowMinimumSize(window : Window*, w : Int*, h : Int*)
  fun set_window_maximum_size = SDL_SetWindowMaximumSize(window : Window*, max_w : Int, max_h : Int)
  fun get_window_maximum_size = SDL_GetWindowMaximumSize(window : Window*, w : Int*, h : Int*)
  fun set_window_bordered = SDL_SetWindowBordered(window : Window*, bordered : Bool)
  fun show_window = SDL_ShowWindow(window : Window*)
  fun hide_window = SDL_HideWindow(window : Window*)
  fun raise_window = SDL_RaiseWindow(window : Window*)
  fun maximize_window = SDL_MaximizeWindow(window : Window*)
  fun minimize_window = SDL_MinimizeWindow(window : Window*)
  fun restore_window = SDL_RestoreWindow(window : Window*)
  fun set_window_fullscreen = SDL_SetWindowFullscreen(window : Window*, flags : UInt32) : Int
  fun get_window_surface = SDL_GetWindowSurface(window : Window*) : Surface*
  fun update_window_surface = SDL_UpdateWindowSurface(window : Window*) : Int
  fun update_window_surface_rects = SDL_UpdateWindowSurfaceRects(window : Window*, rects : Rect*, numrects : Int) : Int
  fun set_window_grab = SDL_SetWindowGrab(window : Window*, grabbed : Bool)
  fun get_window_grab = SDL_GetWindowGrab(window : Window*) : Bool
  fun set_window_brightness = SDL_SetWindowBrightness(window : Window*, brightness : Float) : Int
  fun get_window_brightness = SDL_GetWindowBrightness(window : Window*) : Float
  fun set_window_gamma_ramp = SDL_SetWindowGammaRamp(window : Window*, red : UInt16*, green : UInt16*, blue : UInt16*) : Int
  fun get_window_gamma_ramp = SDL_GetWindowGammaRamp(window : Window*, red : UInt16*, green : UInt16*, blue : UInt16*) : Int
  fun destroy_window = SDL_DestroyWindow(window : Window*)

  fun is_screen_saver_enabled = SDL_IsScreenSaverEnabled() : Bool
  fun enable_screen_saver = SDL_EnableScreenSaver()
  fun disable_screen_saver = SDL_DisableScreenSaver()

  # OpenGL support functions

  fun gl_load_library = SDL_GL_LoadLibrary(path : Char*) : Int
  fun gl_get_proc_address = SDL_GL_GetProcAddress(proc : Char*) : Void*
  fun gl_unload_library = SDL_GL_UnloadLibrary()
  fun gl_extension_supported = SDL_GL_ExtensionSupported(extension : Char*) : Bool
  fun gl_reset_attributes = SDL_GL_ResetAttributes()
  fun gl_set_attribute = SDL_GL_SetAttribute(attr : GLattr, value : Int) : Int
  fun gl_get_attribute = SDL_GL_GetAttribute(attr : GLattr, value : Int*) : Int
  fun gl_create_context = SDL_GL_CreateContext(window : Window*) : GLContext
  fun gl_make_current = SDL_GL_MakeCurrent(window : Window*, context : GLContext) : Int
  fun gl_get_current_window = SDL_GL_GetCurrentWindow() : Window*
  fun gl_get_current_context = SDL_GL_GetCurrentContext() : GLContext
  fun gl_get_drawable_size = SDL_GL_GetDrawableSize(window : Window*, w : Int*, h : Int*) : Int
  fun gl_set_swap_interval = SDL_GL_SetSwapInterval(interval : Int) : Int
  fun gl_get_swap_interval = SDL_GL_GetSwapInterval() : Int
  fun gl_swap_window = SDL_GL_SwapWindow(window : Window*)
  fun gl_delete_context = SDL_GL_DeleteContext(context : GLContext)
end
