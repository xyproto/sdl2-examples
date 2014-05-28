{
  SDL_image:  An example image loading library for use with SDL
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

=====

  SDL2_image header translation for Free Pascal
  https://bitbucket.org/p_daniel/sdl-2-for-free-pascal-compiler

=====

}
unit SDL2_image;

interface

uses SDL2;

{$MACRO ON}
{$INLINE ON}
{$PACKRECORDS C}

{$DEFINE lSDL:=cdecl; external 'SDL2_image'}

{$IFDEF DARWIN}
  {$linkframework SDL2}
  {$linkframework SDL2_image}
{$ENDIF}

const
  SDL_IMAGE_MAJOR_VERSION=2;
  SDL_IMAGE_MINOR_VERSION=0;
  SDL_IMAGE_PATCHLEVEL   =0;

  IMG_INIT_JPG =$00000001;
  IMG_INIT_PNG =$00000002;
  IMG_INIT_TIF =$00000004;
  IMG_INIT_WEBP=$00000008;

procedure SDL_IMAGE_VERSION(x: PSDL_Version); inline;
function IMG_Linked_Version: PSDL_Version; lSDL;

function IMG_Init(flags: longint): longint; lSDL;
procedure IMG_Quit; lSDL;

function IMG_LoadTyped_RW(src: PSDL_RWops; freesrc: longint;
                          const type_: pchar): PSDL_Surface; lSDL;
function IMG_Load(const file_: pchar): PSDL_Surface; lSDL;
function IMG_Load_RW(src: PSDL_RWops; freesrc: longint): PSDL_Surface; lSDL;

function IMG_LoadTexture(renderer: PSDL_Renderer;
                         const file_: pchar): PSDL_Texture; lSDL;
function IMG_LoadTexture_RW(renderer: PSDL_Renderer;
                         src: PSDL_RWops; freesrc: longint): PSDL_Texture; lSDL;
function IMG_LoadTextureTyped_RW(renderer: PSDL_Renderer;
     src: PSDL_RWops; freesrc: longint; const type_: pchar): PSDL_Texture; lSDL;

function IMG_isICO(src: PSDL_RWops): longint; lSDL;
function IMG_isCUR(src: PSDL_RWops): longint; lSDL;
function IMG_isBMP(src: PSDL_RWops): longint; lSDL;
function IMG_isGIF(src: PSDL_RWops): longint; lSDL;
function IMG_isJPG(src: PSDL_RWops): longint; lSDL;
function IMG_isLBM(src: PSDL_RWops): longint; lSDL;
function IMG_isPCX(src: PSDL_RWops): longint; lSDL;
function IMG_isPNG(src: PSDL_RWops): longint; lSDL;
function IMG_isPNM(src: PSDL_RWops): longint; lSDL;
function IMG_isTIF(src: PSDL_RWops): longint; lSDL;
function IMG_isXCF(src: PSDL_RWops): longint; lSDL;
function IMG_isXPM(src: PSDL_RWops): longint; lSDL;
function IMG_isXV(src: PSDL_RWops): longint; lSDL;
function IMG_isWEBP(src: PSDL_RWops): longint; lSDL;

function IMG_LoadICO_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadCUR_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadBMP_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadGIF_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadJPG_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadLBM_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadPCX_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadPNG_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadPNM_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadTGA_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadTIF_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadXCF_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadXPM_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadXV_RW(src: PSDL_RWops): PSDL_Surface; lSDL;
function IMG_LoadWEBP_RW(src: PSDL_RWops): PSDL_Surface; lSDL;

function IMG_ReadXPMFromArray(xpm: ppchar): PSDL_Surface; lSDL;

function IMG_SavePNG(surface: PSDL_Surface; const file_: pchar): longint; lSDL;
function IMG_SavePNG_RW(surface: PSDL_Surface; dst: PSDL_RWops;
                        freedst: longint): longint; lSDL;

function IMG_SetError(const fmt: pchar): longint; cdecl;
                      external 'SDL2' name 'SDL_SetError'; varargs;
function IMG_GetError: pchar; cdecl; external 'SDL2' name 'SDL_GetError';

implementation

procedure SDL_IMAGE_VERSION(x: PSDL_Version); inline;
begin
  x^.major:=SDL_IMAGE_MAJOR_VERSION;
  x^.minor:=SDL_IMAGE_MINOR_VERSION;
  x^.patch:=SDL_IMAGE_PATCHLEVEL;
end;

end.
