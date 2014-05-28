{
  SDL_ttf:  A companion library to SDL for working with TrueType (tm) fonts
  Copyright (C) 2001-2013 Sam Lantinga <slouken@libsdl.org>

=====

  SDL2_ttf header translation for Free Pascal
  https://bitbucket.org/p_daniel/sdl-2-for-free-pascal-compiler

=====

}
unit SDL2_ttf;

interface

uses SDL2;

{$MACRO ON}
{$INLINE ON}
{$PACKRECORDS C}

{$DEFINE lSDL:=cdecl; external 'SDL2_ttf'}

{$IFDEF DARWIN}
  {$linkframework SDL2}
  {$linkframework SDL2_ttf}
{$ENDIF}

const
  SDL_TTF_MAJOR_VERSION=2;
  SDL_TTF_MINOR_VERSION=0;
  SDL_TTF_PATCHLEVEL   =12;

  UNICODE_BOM_NATIVE =$FEFF;
  UNICODE_BOM_SWAPPED=$FFFE;

  TTF_STYLE_NORMAL       =$00;
  TTF_STYLE_BOLD         =$01;
  TTF_STYLE_ITALIC       =$02;
  TTF_STYLE_UNDERLINE    =$04;
  TTF_STYLE_STRIKETHROUGH=$08;

  TTF_HINTING_NORMAL=0;
  TTF_HINTING_LIGHT =1;
  TTF_HINTING_MONO  =2;
  TTF_HINTING_NONE  =3;

type
  PTTF_Font=pointer;

procedure SDL_TTF_VERSION(x: PSDL_Version); inline;
function TTF_Linked_Version: PSDL_Version; lSDL;

procedure TTF_ByteSwappedUNICODE(swapped: longint); lSDL;

function TTF_Init: longint; lSDL;

function TTF_OpenFont(const file_: pchar; ptsize: longint): PTTF_Font; lSDL;
function TTF_OpenFontIndex(const file_: pchar; ptsize, index: longint): PTTF_Font; lSDL;
function TTF_OpenFontRW(src: PSDL_RWops; freesrc, ptsize: longint): PTTF_Font; lSDL;
function TTF_OpenFontIndexRW(src: PSDL_RWops; freesrc, ptsize, index: longint): PTTF_Font; lSDL;

function TTF_GetFontStyle(const font: PTTF_Font): longint; lSDL;
procedure TTF_SetFontStyle(font: PTTF_Font; style: longint); lSDL;
function TTF_GetFontOutline(const font: PTTF_Font): longint; lSDL;
procedure TTF_SetFontOutline(font: PTTF_Font; outline: longint); lSDL;

function TTF_GetFontHinting(const font: PTTF_Font): longint; lSDL;
procedure TTF_SetFontHinting(font: PTTF_Font; hinting: longint); lSDL;

function TTF_FontHeight(const font: PTTF_Font): longint; lSDL;
function TTF_FontAscent(const font: PTTF_Font): longint; lSDL;
function TTF_FontDescent(const font: PTTF_Font): longint; lSDL;
function TTF_FontLineSkip(const font: PTTF_Font): longint; lSDL;

function TTF_GetFontKerning(const font: PTTF_Font): longint; lSDL;
procedure TTF_SetFontKerning(font: PTTF_Font; allowed: longint); lSDL;

function TTF_FontFaces(const font: PTTF_Font): longint; lSDL;

function TTF_FontFaceIsFixedWidth(const font: PTTF_Font): longint; lSDL;
function TTF_FontFaceFamilyName(const font: PTTF_Font): pchar; lSDL;
function TTF_FontFaceStyleName(const font: PTTF_Font): pchar; lSDL;

function TTF_GlyphIsProvided(const font: PTTF_Font; ch: Uint16): longint; lSDL;

function TTF_GlyphMetrics(font: PTTF_Font; ch: Uint16;
                      minx, maxx, miny, maxy, advance: plongint): longint; lSDL;

function TTF_SizeText(font: PTTF_Font; const text: pchar;
                      w, h: plongint): longint; lSDL;
function TTF_SizeUTF8(font: PTTF_Font; const text: pchar;
                      w, h: plongint): longint; lSDL;
function TTF_SizeUNICODE(font: PTTF_Font; const text: PUint16;
                         w, h: plongint): longint; lSDL;

function TTF_RenderText_Solid(font: PTTF_Font; const text: pchar;
                              fg: TSDL_Color): PSDL_Surface; lSDL;
function TTF_RenderUTF8_Solid(font: PTTF_Font; const text: pchar;
                              fg: TSDL_Color): PSDL_Surface; lSDL;
function TTF_RenderUNICODE_Solid(font: PTTF_Font; const text: PUint16;
                                 fg: TSDL_Color): PSDL_Surface; lSDL;

function TTF_RenderGlyph_Solid(font: PTTF_Font; ch: Uint16;
                               fg: TSDL_Color): PSDL_Surface; lSDL;

function TTF_RenderText_Shaded(font: PTTF_Font; const text: pchar;
                               fg, bg: TSDL_Color): PSDL_Surface; lSDL;
function TTF_RenderUTF8_Shaded(font: PTTF_Font; const text: pchar;
                               fg, bg: TSDL_Color): PSDL_Surface; lSDL;
function TTF_RenderUNICODE_Shaded(font: PTTF_Font; const text: PUint16;
                               fg, bg: TSDL_Color): PSDL_Surface; lSDL;

function TTF_RenderGlyph_Shaded(font: PTTF_Font; ch: Uint16;
                                fg, bg: TSDL_Color): PSDL_Surface; lSDL;

function TTF_RenderText_Blended(font: PTTF_Font; const text: pchar;
                                fg: TSDL_Color): PSDL_Surface; lSDL;
function TTF_RenderUTF8_Blended(font: PTTF_Font; const text: pchar;
                                fg: TSDL_Color): PSDL_Surface; lSDL;
function TTF_RenderUNICODE_Blended(font: PTTF_Font; const text: PUint16;
                                   fg: TSDL_Color): PSDL_Surface; lSDL;

function TTF_RenderText_Blended_Wrapped(font: PTTF_Font; const text: pchar;
                        fg: TSDL_Color; wrapLength: Uint32): PSDL_Surface; lSDL;
function TTF_RenderUTF8_Blended_Wrapped(font: PTTF_Font; const text: pchar;
                         fg: TSDL_Color;wrapLength: Uint32): PSDL_Surface; lSDL;
function TTF_RenderUNICODE_Blended_Wrapped(font: PTTF_Font; const text: PUint16;
                        fg: TSDL_Color; wrapLength: Uint32): PSDL_Surface; lSDL;

function TTF_RenderGlyph_Blended(font: PTTF_Font; ch: Uint16;
                                 fg: TSDL_Color): PSDL_Surface; lSDL;

procedure TTF_CloseFont(font: PTTF_Font); lSDL;
procedure TTF_Quit; lSDL;

function TTF_WasInit: longint; lSDL;

function TTF_GetFontKerningSize(font: PTTF_Font;
                                prev_index, index: longint): longint; lSDl;

function TTF_SetError(const fmt: pchar): longint; cdecl;
                      external 'SDL2' name 'SDL_SetError'; varargs;
function TTF_GetError: pchar; cdecl; external 'SDL2' name 'SDL_GetError';

implementation

procedure SDL_TTF_VERSION(x: PSDL_Version); inline;
begin
  x^.major:=SDL_TTF_MAJOR_VERSION;
  x^.minor:=SDL_TTF_MINOR_VERSION;
  x^.patch:=SDL_TTF_PATCHLEVEL;
end;

end.
