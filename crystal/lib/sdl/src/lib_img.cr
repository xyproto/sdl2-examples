require "./lib_sdl"

@[Link("SDL2_image")]
lib LibIMG
  alias Bool = LibC::Int

  alias Char = LibC::Char
  alias Int = LibC::Int
  alias SizeT = LibC::SizeT

  alias Surface = LibSDL::Surface
  alias Texture = LibSDL::Texture
  alias Renderer = LibSDL::Renderer
  alias RWops = LibSDL::RWops

  VERSION = {% `pkg-config SDL2_image --modversion`.strip %}
  MAJOR = {% VERSION.split('.')[0] %}
  MINOR = {% VERSION.split('.')[1] %}
  PATCH = {% VERSION.split('.')[2] %}

  @[Flags]
  enum Init
    JPG  = 0x00000001
    PNG  = 0x00000002
    TIF  = 0x00000004
    WEBP = 0x00000008
  end

  fun init = IMG_Init(flags : Init) : Int
  fun quit = IMG_Quit()

  fun load = IMG_Load(file : Char*) : Surface*
  fun load_rw = IMG_Load_RW(src : RWops*, freesrc : Int) : Surface*
  fun load_typed_rw = IMG_LoadTyped_RW(src : RWops*, freesrc : Int, type : Char*) : Surface*

  fun load_texture = IMG_LoadTexture(renderer : Renderer*, file : Char*) : Texture*
  fun load_texture_rw = IMG_LoadTexture_RW(renderer : Renderer*, src : RWops*, freesrc : Int) : Texture*
  fun load_texture_typed_rw = IMG_LoadTextureTyped_RW(renderer : Renderer*, src : RWops*, freesrc : Int, type : Char*) : Texture*

  fun is_ico = IMG_isICO(src : RWops*) : Int
  fun is_cur = IMG_isCUR(src : RWops*) : Int
  fun is_bmp = IMG_isBMP(src : RWops*) : Int
  fun is_gif = IMG_isGIF(src : RWops*) : Int
  fun is_jpg = IMG_isJPG(src : RWops*) : Int
  fun is_lbm = IMG_isLBM(src : RWops*) : Int
  fun is_pcx = IMG_isPCX(src : RWops*) : Int
  fun is_png = IMG_isPNG(src : RWops*) : Int
  fun is_pnm = IMG_isPNM(src : RWops*) : Int
  fun is_tif = IMG_isTIF(src : RWops*) : Int
  fun is_xcf = IMG_isXCF(src : RWops*) : Int
  fun is_xpm = IMG_isXPM(src : RWops*) : Int
  fun is_xv = IMG_isXV(src : RWops*) : Int
  fun is_webp = IMG_isWEBP(src : RWops*) : Int

  fun load_ico_rw = IMG_LoadICO_RW(src : RWops*) : Surface*
  fun load_cur_rw = IMG_LoadCUR_RW(src : RWops*) : Surface*
  fun load_bmp_rw = IMG_LoadBMP_RW(src : RWops*) : Surface*
  fun load_gif_rw = IMG_LoadGIF_RW(src : RWops*) : Surface*
  fun load_jpg_rw = IMG_LoadJPG_RW(src : RWops*) : Surface*
  fun load_lbm_rw = IMG_LoadLBM_RW(src : RWops*) : Surface*
  fun load_pcx_rw = IMG_LoadPCX_RW(src : RWops*) : Surface*
  fun load_png_rw = IMG_LoadPNG_RW(src : RWops*) : Surface*
  fun load_pnm_rw = IMG_LoadPNM_RW(src : RWops*) : Surface*
  fun load_tga_rw = IMG_LoadTGA_RW(src : RWops*) : Surface*
  fun load_tif_rw = IMG_LoadTIF_RW(src : RWops*) : Surface*
  fun load_xcf_rw = IMG_LoadXCF_RW(src : RWops*) : Surface*
  fun load_xpm_rw = IMG_LoadXPM_RW(src : RWops*) : Surface*
  fun load_xv_rw = IMG_LoadXV_RW(src : RWops*) : Surface*
  fun load_webp_rw = IMG_LoadWEBP_RW(src : RWops*) : Surface*

  fun read_xpm_from_array = IMG_ReadXPMFromArray(xpm : Char**) : Surface*

  fun save_png = IMG_SavePNG(surface : Surface*, file : Char*) : Int
  fun save_png_rw = IMG_SavePNG_RW(surface : Surface*, dst : RWops*, freedst : Int) : Int
end
