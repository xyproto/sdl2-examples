{
  SDL_mixer:  An audio mixer library based on the SDL library
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

=====

  SDL2_mixer header translation for Free Pascal
  https://bitbucket.org/p_daniel/sdl-2-for-free-pascal-compiler

=====

}
unit SDL2_mixer;

interface

uses SDL2;

{$MACRO ON}
{$INLINE ON}
{$PACKRECORDS C}

{$DEFINE lSDL:=cdecl; external 'SDL2_mixer'}

{$IFDEF DARWIN}
  {$linkframework SDL2}
  {$linkframework SDL2_mixer}
{$ENDIF}

const
  SDL_MIXER_MAJOR_VERSION=2;
  SDL_MIXER_MINOR_VERSION=0;
  SDL_MIXER_PATCHLEVEL   =0;

  MIX_INIT_FLAC      =$00000001;
  MIX_INIT_MOD       =$00000002;
  MIX_INIT_MODPLUG   =$00000004;
  MIX_INIT_MP3       =$00000008;
  MIX_INIT_OGG       =$00000010;
  MIX_INIT_FLUIDSYNTH=$00000020;

  MIX_CHANNELS=8;

  MIX_DEFAULT_FREQUENCY=22050;

{$IFDEF FPC_LITTLE_ENDIAN}
  MIX_DEFAULT_FORMAT=AUDIO_S16LSB;
{$ELSE}
  MIX_DEFAULT_FORMAT=AUDIO_S16MSB;
{$ENDIF}

  MIX_DEFAULT_CHANNELS=2;
  MIX_MAX_VOLUME=128;

  MIX_CHANNEL_POST=-2;

  MIX_EFFECTSMAXSPEED='MIX_EFFECTSMAXSPEED';

type
  PMix_Chunk=^TMix_Chunk;
  TMix_Chunk=record
    allocated: longint;
    abuf: PUint8;
    alen: Uint32;
    volume: Uint8;
  end;

  TMix_Fading=(
    MIX_NO_FADING,
    MIX_FADING_OUT,
    MIX_FADING_IN);

  TMix_MusicType=(
    MUS_NONE,
    MUS_CMD,
    MUS_WAV,
    MUS_MOD,
    MUS_MID,
    MUS_OGG,
    MUS_MP3,
    MUS_MP3_MAD,
    MUS_FLAC,
    MUS_MODPLUG);

  PMix_Music=pointer;

procedure SDL_MIXER_VERSION(x: PSDL_Version); inline;

function Mix_Init(flags: longint): longint; lSDL;
procedure Mix_Quit; lSDL;

function Mix_OpenAudio(frequency: longint; format: Uint16; channels,
                       chunksize: longint): longint; lSDL;
function Mix_AllocateChannels(numchans: longint): longint; lSDL;
function Mix_QuerySpec(frequency: plongint; format: PUint16;
                       channels: plongint): longint; lSDL;

function Mix_LoadWAV_RW(src: PSDL_RWops; freesrc: longint): PMix_Chunk; lSDL;
function Mix_LoadWAV(file_: pchar): PMix_Chunk; inline;
function Mix_LoadMUS(const file_: pchar): PMix_Music; lSDL;
function Mix_LoadMUS_RW(src: PSDL_RWops; freesrc: longint): PMix_Music; lSDL;
function Mix_LoadMUSType_RW(src: PSDL_RWops; type_: TMix_MusicType;
                            freesrc: longint): PMix_Music; lSDL;
function Mix_QuickLoad_WAV(mem: PUint8): PMix_Chunk; lSDL;
function Mix_QuickLoad_RAW(mem: PUint8; len: Uint32): PMix_Chunk; lSDL;

procedure Mix_FreeChunk(chunk: PMix_Chunk); lSDL;
procedure Mix_FreeMusic(music: PMix_Music); lSDL;

function Mix_GetNumChunkDecoders: longint; lSDL;
function Mix_GetChunkDecoder(index: longint): pchar; lSDL;
function Mix_GetNumMusicDecoders: longint; lSDL;
function Mix_GetMusicDecoder(index: longint): pchar; lSDL;
function Mix_GetMusicType(const music: PMix_Music): TMix_MusicType; lSDL;

type
  TMixFunction=procedure(udata: pointer; stream: PUint8; lon: longint); cdecl;

procedure Mix_SetPostMix(mix_func: TMixFunction; arg: pointer); lSDL;
procedure Mix_HookMusic(mix_func: TMixFunction; arg: pointer); lSDL;
procedure Mix_HookMusicFinished(music_finished: pointer); lSDL;
function Mix_GetMusicHookData: pointer; lSDL;

type
  TChannelFinished=procedure(channel: longint); cdecl;

procedure Mix_ChannelFinished(channel_finished: TChannelFinished); lSDL;

type
  TMix_EffectFunc_t=procedure(chan: longint; stream: pointer;
                                len: longint; udata: pointer); cdecl;
  TMix_EffectDone_t=procedure(chan: longint; udata: pointer); cdecl;

function Mix_RegisterEffect(chan: longint; f: TMix_EffectFunc_t;
                            d: TMix_EffectDone_t; arg: pointer): longint; lSDL;
function Mix_UnregisterEffect(channel: longint;
                              f: TMix_EffectFunc_t): longint; lSDL;
function Mix_UnregisterAllEffects(channel: longint): longint; lSDL;

function Mix_SetPanning(channel: longint; left, right: Uint8): longint; lSDL;
function Mix_SetPosition(channel: longint; angle: Sint16;
                         distance: Uint8): longint; lSDL;
function Mix_SetDistance(channel: longint; distance: Uint8): longint; lSDL;
function Mix_SetReverseStereo(channel, flip: longint): longint; lSDL;
function Mix_ReserveChannels(num: longint): longint; lSDL;

function Mix_GroupChannel(which, tag: longint): longint; lSDL;
function Mix_GroupChannels(from, to_, tag: longint): longint; lSDL;
function Mix_GroupAvailable(tag: longint): longint; lSDL;
function Mix_GroupCount(tag: longint): longint; lSDL;
function Mix_GroupOldest(tag: longint): longint; lSDL;
function Mix_GroupNewer(tag: longint): longint; lSDL;

function Mix_PlayChannel(channel: longint; chunk: PMix_Chunk;
                         loops: longint): longint; inline;
function Mix_PlayChannelTimed(channel: longint; chunk: PMix_Chunk;
                              loops, ticks: longint): longint; lSDL;
function Mix_PlayMusic(music: PMix_Music; loops: longint): longint; lSDL;

function Mix_FadeInMusic(music: PMix_Music; loops, ms: longint): longint; lSDL;
function Mix_FadeInMusicPos(music: PMix_Music; loops,
                            ms: longint; position: double): longint; lSDL;
function Mix_FadeInChannel(channel: longint; chunk: PMix_Chunk;
                           loops, ms: longint): longint; inline;
function Mix_FadeInChannelTimed(channel: longint; chunk: PMix_Chunk;
                                loops, ms, ticks: longint): longint; lSDL;

function Mix_Volume(channel, volume: longint): longint; lSDL;
function Mix_VolumeChunk(chunk: PMix_Chunk; volume: longint): longint; lSDL;
function Mix_VolumeMusic(volume: longint): longint; lSDL;

function Mix_HaltChannel(channel: longint): longint; lSDL;
function Mix_HaltGroup(tag: longint): longint; lSDL;
function Mix_HaltMusic: longint; lSDL;

function Mix_ExpireChannel(channel, ticks: longint): longint; lSDL;

function Mix_FadeOutChannel(which, ms: longint): longint; lSDL;
function Mix_FadeOutGroup(tag, ms: longint): longint; lSDL;
function Mix_FadeOutMusic(ms: longint): longint; lSDL;

function Mix_FadingMusic: TMix_Fading; lSDL;
function Mix_FadingChannel(which: longint): TMix_Fading; lSDL;

procedure Mix_Pause(channel: longint); lSDL;
procedure Mix_Resume(channel: longint); lSDL;
function Mix_Paused(channel: longint): longint; lSDL;

procedure Mix_PauseMusic; lSDL;
procedure Mix_ResumeMusic; lSDL;
procedure Mix_RewindMusic; lSDL;
function Mix_PausedMusic: longint; lSDL;

function Mix_SetMusicPosition(position: double): longint; lSDL;

function Mix_Playing(channel: longint): longint; lSDL;
function Mix_PlayingMusic: longint; lSDL;

function Mix_SetMusicCMD(const command: pchar): longint; lSDL;

function Mix_SetSynchroValue(value: longint): longint; lSDL;
function Mix_GetSynchroValue: longint; lSDL;

function Mix_SetSoundFonts(const paths: pchar): longint; lSDL;
function Mix_GetSoundFonts: pchar; lSDL;

type
  TEachSoundFont_f=function(c: pchar; p: pointer): longint; cdecl;

function Mix_EachSoundFont(fn: TEachSoundFont_f; data: pchar): longint; lSDL;

function Mix_GetChunk(channel: longint): PMix_Chunk; lSDL;

procedure Mix_CloseAudio; lSDL;

function Mix_SetError(const fmt: pchar): longint; cdecl;
                      external 'SDL2' name 'SDL_SetError'; varargs;
function Mix_GetError: pchar; cdecl; external 'SDL2' name 'SDL_GetError';

implementation

procedure SDL_MIXER_VERSION(x: PSDL_Version); inline;
begin
  x^.major:=SDL_MIXER_MAJOR_VERSION;
  x^.minor:=SDL_MIXER_MINOR_VERSION;
  x^.patch:=SDL_MIXER_PATCHLEVEL;
end;

function Mix_LoadWAV(file_: pchar): PMix_Chunk; inline;
begin
  Mix_LoadWAV:=Mix_LoadWAV_RW(SDL_RWFromFile(file_, 'r'), 1);
end;

function Mix_PlayChannel(channel: longint; chunk: PMix_Chunk;
                         loops: longint): longint; inline;
begin
  Mix_PlayChannel:=Mix_PlayChannelTimed(channel, chunk, loops, -1);
end;

function Mix_FadeInChannel(channel: longint; chunk: PMix_Chunk;
                           loops, ms: longint): longint; inline;
begin
  Mix_FadeInChannel:=Mix_FadeInChannelTimed(channel, chunk, loops, ms, -1);
end;

end.
