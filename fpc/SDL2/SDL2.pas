{
  Simple DirectMedia Layer
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

=====

  SDL2 headers translation for Free Pascal
  https://bitbucket.org/p_daniel/sdl-2-for-free-pascal-compiler

=====

  Not included:
    • SDL_atomic
    • SDL_assert
    • SDL_main
    • SDL_syswm
    • SDL_endian:
        SDL_(LIL/BIG)_ENDIAN -> FPC_(LITTLE/BIG)_ENDIAN (or ENDIAN_(LITTLE/BIG))
        SDL_SwapXX -> SwapEndian/LEToN/BEToN/NToLE/NToBE

=====

}
unit SDL2;

interface

{$MACRO ON}
{$INLINE ON}
{$PACKRECORDS C}

{$DEFINE lSDL:=cdecl; external 'SDL2'}

{$IFDEF DARWIN}
  {$linkframework SDL2}
{$ENDIF}

{$IFNDEF WINDOWS} //STDIO disabled on WIN by default (see SDL_config_windows.h)
  {$DEFINE HAVE_STDIO_H}
{$ENDIF}

//=====SDL_STDINC=====

type
  SDL_bool = longbool;

  PSint8 = ^Sint8;
  Sint8 = shortint;
  PUint8 = ^Uint8;

  PSint16 = ^Sint16;
  Sint16 = smallint;
  PUint16 = ^Uint16;

  Sint32 = longint;
  PUint32 = ^Uint32; //longword

  PUint64 = ^Uint64; //qword
  PSint64 = ^Sint64;
  Sint64 = int64;

  size_t = PtrUInt;
  psize_t = ^size_t;
  TSDL_iconv_t = pointer;
  pwchar_t = plongint;

function SDL_malloc(size: size_t): pointer; lSDL;
function SDL_calloc(nmemb, size: size_t): pointer; lSDL;
function SDL_realloc(mem: pointer; size: size_t): pointer; lSDL;
procedure SDL_free(mem: pointer); lSDL;

function SDL_getenv(name: pchar): pchar; lSDL;
function SDL_setenv(const name, value: pchar;
                    overwrite: longint): longint; lSDL;

type
  comparefn=function(a1, a2: pointer): longint; cdecl;

procedure SDL_qsort(base: pointer;
                    nmemb, size: size_t; compare: comparefn); lSDL;

function SDL_memset(dst: pointer; c: longint; len: size_t): pointer; lSDL;
function SDL_memcpy(dst: pointer; const src: pointer;
                    len: size_t): pointer; lSDL;
function SDL_memmove(dst: pointer; const src: pointer;
                     len: size_t): pointer; lSDL;
function SDL_memcmp(const s1, s2: pointer; len: size_t): longint; lSDL;

function SDL_wcslen(const wstr: pwchar_t): size_t; lSDL;
function SDL_wcslcpy(dst: pwchar_t; const src: pwchar_t;
                     maxlen: size_t): size_t; lSDL;
function SDL_wcslcat(dst: pwchar_t; const src: pwchar_t;
                     maxlen: size_t): size_t; lSDL;

function SDL_strlen(const str: pchar): size_t; lSDL;
function SDL_strlcpy(dst: pchar; const src: pchar;
                     maxlen: size_t): size_t; lSDL;
function SDL_utf8strlcpy(dst: pchar; const src: pchar;
                         dst_bytes: size_t): size_t; lSDL;
function SDL_strlcat(dst: pchar; const src: pchar;
                     maxlen: size_t): size_t; lSDL;
function SDL_strdup(const str: pchar): pchar; lSDL;
function SDL_strrev(str: pchar): pchar; lSDL;
function SDL_strupr(str: pchar): pchar; lSDL;
function SDL_strlwr(str: pchar): pchar; lSDL;
function SDL_strchr(const str: pchar; c: longint): pchar; lSDL;
function SDL_strrchr(const str: pchar; c: longint): pchar; lSDL;
function SDL_strstr(const haystack, needle: pchar): pchar; lSDL;

function SDL_itoa(value: longint; str: pchar; radix: longint): pchar; lSDL;
function SDL_uitoa(value: longword; str: pchar; radix: longint): pchar; lSDL;
function SDL_ltoa(value: longint; str: pchar; radix: longint): pchar; lSDL;
function SDL_ultoa(value: longword; str: pchar; radix: longint): pchar; lSDL;
function SDL_lltoa(value: Sint64; str: pchar; radix: longint): pchar; lSDL;
function SDL_ulltoa(value: Uint64; str: pchar; radix: longint): pchar; lSDL;

function SDL_atoi(const str: pchar): longint; lSDL;
function SDL_atof(const str: pchar): double; lSDL;
function SDL_strtol(const str: pchar; endp: ppchar;
                    base: longint): longint; lSDL;
function SDL_strtoul(const str: pchar; endp: ppchar;
                     base: longint): longword; lSDL;
function SDL_strtoll(const str: pchar; endp: ppchar;
                     base: longint): Sint64; lSDL;
function SDL_strtoull(const str: pchar; endp: ppchar;
                      base: longint): Uint64; lSDL;
function SDL_strtod(const str: pchar; endp: ppchar): double; lSDL;

function SDL_strcmp(const str1, str2: pchar): longint; lSDL;
function SDL_strncmp(const str1, str2: pchar; maxlen: size_t): longint; lSDL;
function SDL_strcasecmp(const str1, str2: pchar): longint; lSDL;
function SDL_strncasecmp(const str1, str2: pchar; len: size_t): longint; lSDL;

function SDL_sscanf(const text, fmt: pchar): longint; lSDL; varargs;
function SDL_snprintf(text: pchar; maxlen: size_t;
                      const fmt: pchar): longint; lSDL; varargs;

function SDL_abs(x: longint): longint; lSDL;

function SDL_acos(x: double): double; lSDL;
function SDL_asin(x: double): double; lSDL;
function SDL_atan(x: double): double; lSDL;
function SDL_atan2(x, y: double): double; lSDL;
function SDL_ceil(x: double): double; lSDL;
function SDL_copysign(x, y: double): double; lSDL;
function SDL_cos(x: double): double; lSDL;
function SDL_cosf(x: single): single; lSDl;
function SDL_fabs(x: double): double; lSDL;
function SDL_floor(x: double): double; lSDL;
function SDL_log(x: double): double; lSDL;
function SDL_pow(x, y: double): double; lSDL;
function SDL_scalbn(x: double; n: longint): double; lSDL;
function SDL_sin(x: double): double; lSDL;
function SDL_sinf(x: single): single; lSDL;
function SDL_sqrt(x: double): double; lSDL;

function SDL_iconv_open(const tocode, fromcode: pchar): TSDL_iconv_t; lSDL;
function SDL_iconv_close(cd: TSDL_iconv_t): longint; lSDL;
function SDL_iconv(cd: TSDL_iconv_t; const inbuf: ppchar; inbytesleft: psize_t;
                   outbuf: ppchar; outbytesleft: psize_t): size_t; lSDL;
function SDL_iconv_string(const tocode, fromcode,
                          inbuf: pchar; inbytesleft: size_t): pchar; lSDl;
function SDL_iconv_utf8_locale(S: pchar): pchar; inline;
function SDL_iconv_utf8_ucs2(S: pchar): pchar; inline;
function SDL_iconv_utf8_ucs4(S: pchar): pchar; inline;


//=====SDL_BITS=====

function SDL_MostSignificantBitIndex32(x: Uint32): longint; inline;

//=====SDL=====

const
  SDL_INIT_TIMER         =$00000001;
  SDL_INIT_AUDIO         =$00000010;
  SDL_INIT_VIDEO         =$00000020;
  SDL_INIT_JOYSTICK      =$00000200;
  SDL_INIT_HAPTIC        =$00001000;
  SDL_INIT_GAMECONTROLLER=$00002000;
  SDL_INIT_NOPARACHUTE   =$00100000;
  SDL_INIT_EVERYTHING=SDL_INIT_TIMER or
                      SDL_INIT_AUDIO or
                      SDL_INIT_VIDEO or
                      SDL_INIT_JOYSTICK or
                      SDL_INIT_HAPTIC or
                      SDL_INIT_GAMECONTROLLER;

function SDL_Init(flags: Uint32): longint; lSDL;
function SDL_InitSubSystem(flags: Uint32): longint; lSDL;
procedure SDL_QuitSubSystem(flags: Uint32); lSDL;
function SDL_WasInit(flags: Uint32): longint; lSDL;
procedure SDL_Quit; lSDL;

//=====SDL_VERSION=====

const
  SDL_MAJOR_VERSION=2;
  SDL_MINOR_VERSION=0;
  SDL_PATCHLEVEL   =3;

type
  PSDL_version=^TSDL_version;
  TSDL_version=record
    major,
    minor,
    patch: Uint8;
  end;

procedure SDL_VERSION(x: PSDL_Version); inline;

function SDL_VERSIONNUM(X, Y, Z: longint): longint; inline;
function SDL_COMPILEDVERSION: longint; inline;
function SDL_VERSION_ATLEAST(X, Y, Z: longint): boolean; inline;

procedure SDL_GetVersion(ver: PSDL_version); lSDL;
function SDL_GetRevision: pchar; lSDL;
function SDL_GetRevisionNumber: longint; lSDL;

//=====SDL_FILESYSTEM=====

function SDL_GetBasePath: pchar; lSDL;
function SDL_GetPrefPath(const org, app: pchar): pchar; lSDL;

//======SDL_ERROR=====

function SDL_SetError(const fmt: pchar): longint; lSDL; varargs;
function SDL_GetError: pchar; lSDL;
procedure SDL_ClearError; lSDL;

//=====SDL_LOG=====

const
  SDL_LOG_CATEGORY_APPLICATION=0;
  SDL_LOG_CATEGORY_ERROR=1;
  SDL_LOG_CATEGORY_ASSERT=2;
  SDL_LOG_CATEGORY_SYSTEM=3;
  SDL_LOG_CATEGORY_AUDIO=4;
  SDL_LOG_CATEGORY_VIDEO=5;
  SDL_LOG_CATEGORY_RENDER=6;
  SDL_LOG_CATEGORY_INPUT=7;
  SDL_LOG_CATEGORY_TEST=8;
  SDL_LOG_CATEGORY_CUSTOM=19;

type
  TSDL_LogPriority=(
    SDL_LOG_PRIORITY_VERBOSE=1,
    SDL_LOG_PRIORITY_DEBUG,
    SDL_LOG_PRIORITY_INFO,
    SDL_LOG_PRIORITY_WARN,
    SDL_LOG_PRIORITY_ERROR,
    SDL_LOG_PRIORITY_CRITICAL,
    SDL_NUM_LOG_PRIORITIES);

  PSDL_LogOutputFunction=^TSDL_LogOutputFunction;
  TSDL_LogOutputFunction=procedure(userdata: pointer; category: longint;
                             priority: TSDL_LogPriority; message: pchar); cdecl;

procedure SDL_LogSetAllPriority(priority: TSDL_LogPriority); lSDL;
procedure SDL_LogSetPriority(category: longint;
                             priority: TSDL_LogPriority); lSDL;
function SDL_GetPriority(category: longint): TSDL_LogPriority; lSDL;
procedure SDL_LogResetPriorities; lSDL;
procedure SDL_Log(fmt: pchar); lSDL; varargs;
procedure SDL_LogVerbose(category: longint; fmt: pchar); lSDL; varargs;
procedure SDL_LogDebug(category: longint; fmt: pchar); lSDL; varargs;
procedure SDL_LogInfo(category: longint; fmt: pchar); lSDL; varargs;
procedure SDL_LogWarn(category: longint; fmt: pchar); lSDL; varargs;
procedure SDL_LogError(category: longint; fmt: pchar); lSDL; varargs;
procedure SDL_LogCritical(category: longint; fmt: pchar); lSDL; varargs;
procedure SDL_LogMessage(category: longint; priority: TSDL_LogPriority;
                         fmt: pchar); lSDL; varargs;
procedure SDL_LogGetOutputFunction(callback: PSDL_LogOutputFunction;
                                   userdata: ppointer); lSDL;
procedure SDL_LogSetOutputFunction(callback: TSDL_LogOutputFunction;
                                   userdata: pointer); lSDL;

//=====SDL_HINTS=====

const
  SDL_HINT_FRAMEBUFFER_ACCELERATION='SDL_FRAMEBUFFER_ACCELERATION';
  SDL_HINT_RENDER_DRIVER='SDL_RENDER_DRIVER';
  SDL_HINT_RENDER_OPENGL_SHADERS='SDL_RENDER_OPENGL_SHADERS';
  SDL_HINT_RENDER_DIRECT3D_THREADSAFE='SDL_RENDER_DIRECT3D_THREADSAFE';
  SDL_HINT_RENDER_SCALE_QUALITY='SDL_RENDER_SCALE_QUALITY';
  SDL_HINT_RENDER_VSYNC='SDL_RENDER_VSYNC';
  SDL_HINT_VIDEO_ALLOW_SCREENSAVER='SDL_VIDEO_ALLOW_SCREENSAVER';
  SDL_HINT_VIDEO_X11_XVIDMODE='SDL_VIDEO_X11_XVIDMODE';
  SDL_HINT_VIDEO_X11_XINERAMA='SDL_VIDEO_X11_XINERAMA';
  SDL_HINT_VIDEO_X11_XRANDR='SDL_VIDEO_X11_XRANDR';
  SDL_HINT_GRAB_KEYBOARD='SDL_GRAB_KEYBOARD';
  SDL_HINT_MOUSE_RELATIVE_MODE_WARP='SDL_MOUSE_RELATIVE_MODE_WARP';
  SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS='SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS';
  SDL_HINT_IDLE_TIMER_DISABLED='SDL_IOS_IDLE_TIMER_DISABLED';
  SDL_HINT_ORIENTATIONS='SDL_IOS_ORIENTATIONS';
  SDL_HINT_ACCELEROMETER_AS_JOYSTICK='SDL_ACCELEROMETER_AS_JOYSTICK';
  SDL_HINT_XINPUT_ENABLED='SDL_XINPUT_ENABLED';
  SDL_HINT_GAMECONTROLLERCONFIG='SDL_GAMECONTROLLERCONFIG';
  SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS='SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS';
  SDL_HINT_ALLOW_TOPMOST='SDL_ALLOW_TOPMOST';
  SDL_HINT_TIMER_RESOLUTION='SDL_TIMER_RESOLUTION';
  SDL_HINT_VIDEO_HIGHDPI_DISABLED='SDL_VIDEO_HIGHDPI_DISABLED';
  SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK='SDL_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK';
  SDL_HINT_VIDEO_WIN_D3DCOMPILER='SDL_VIDEO_WIN_D3DCOMPILER';
  SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT='SDL_VIDEO_WINDOW_SHARE_PIXEL_FORMAT';
  SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES='SDL_VIDEO_MAC_FULLSCREEN_SPACES';

type
  TSDL_HintPriority=(
    SDL_HINT_DEFAULT,
    SDL_HINT_NORMAL,
    SDL_HINT_OVERRIDE);

  TSDL_HintCallback=procedure(userdata: pointer;
                              name, oldValue, newValue: pchar); cdecl;

function SDL_SetHintWithPriority(name, value: pchar;
                                 priority: TSDL_HintPriority): SDL_bool; lSDL;
function SDL_SetHint(name, value: pchar): SDL_bool; lSDL;
function SDL_GetHint(name: pchar): pchar; lSDL;
procedure SDL_AddHintCallback(name: pchar; callback: TSDL_HintCallback;
                              userdata: pointer); lSDL;
procedure SDL_DelHintCallback(name: pchar; callback: TSDL_HintCallback;
                              userdata: pointer); lSDL;
procedure SDL_ClearHints; lSDL;

//=====SDL_CPUINFO=====

function SDL_GetCPUCount: longint; lSDL;
function SDL_GetCPUCacheLineSize: longint; lSDL;
function SDL_HasRDTSC: SDL_bool; lSDL;
function SDL_HasAltiVec: SDL_bool; lSDL;
function SDL_HasMMX: SDL_bool; lSDL;
function SDL_Has3DNow: SDL_bool; lSDL;
function SDL_HasSSE: SDL_bool; lSDL;
function SDL_HasSSE2: SDL_bool; lSDL;
function SDL_HasSSE3: SDL_bool; lSDL;
function SDL_HasSSE41: SDL_bool; lSDL;
function SDL_HasSSE42: SDL_bool; lSDL;
function SDL_HasAVX: SDL_bool; lSDL;
function SDL_GetSystemRAM: longint; lSDL;

//=====SDL_CLIPBOARD=====

function SDL_SetClipboardText(const text: pchar): longint; lSDL;
function SDL_GetClipboardText: pchar; lSDL;
function SDL_HasClipboardText: SDL_bool; lSDL;

//=====SDL_LOADSO=====

function SDL_LoadObject(const sofile: pchar): pointer; lSDL;
function SDL_LoadFunction(handle: pointer; const name: pchar): pointer; lSDL;
procedure SDL_UnloadObject(handle: pointer); lSDL;

//=====SDL_PLATFORM=====

function SDL_GetPlatform: pchar; lSDL;

//=====SDL_POWER=====

type
  TSDL_PowerState=(
    SDL_POWERSTATE_UNKNOWN,
    SDL_POWERSTATE_ON_BATTERY,
    SDL_POWERSTATE_NO_BATTERY,
    SDL_POWERSTATE_CHARGING,
    SDL_POWERSTATE_CHARGED);

function SDL_GetPowerInfo(secs, pct: plongint): TSDL_PowerState; lSDL;

//=====SDL_TIMER=====

type
  TSDL_TimerCallback=function(interval: Uint32; param: pointer): Uint32;
  TSDL_TimerID=longint;

function SDL_GetTicks: Uint32; lSDL;
function SDL_TICKS_PASSED(a, b: Uint32): SDL_bool; inline;
function SDL_GetPerformanceCounter: Uint64; lSDL;
function SDL_GetPerformanceFrequency: Uint64; lSDL;
procedure SDL_Delay(ms: Uint32); lSDL;

function SDL_AddTimer(interval: Uint32; callback: TSDL_TimerCallback;
                      param: pointer): TSDL_TimerID; lSDL;
function SDL_RemoveTimer(id: TSDL_TimerID): SDL_bool; lSDL;

//=====SDL_MUTEX=====

const
  SDL_MUTEX_TIMEDOUT=1;
  SDL_MUTEX_MAXWAIT=0;

type
  PSDL_Mutex=pointer;
  PSDL_Sem=pointer;
  PSDL_Cond=pointer;

function SDL_CreateMutex: PSDL_Mutex; lSDL;
function SDL_MutexP(mutex: PSDL_Mutex): longint; inline;
function SDL_LockMutex(mutex: PSDL_Mutex): longint; lSDL;
function SDL_TryLockMutex(mutex: PSDL_Mutex): longint; lSDL;
function SDL_MutexV(mutex: PSDL_Mutex): longint; inline;
function SDL_UnlockMutex(mutex: PSDL_Mutex): longint; lSDL;
procedure SDL_DestroyMutex(mutex: PSDL_Mutex); lSDL;

function SDL_CreateSemaphore(initial_value: Uint32): PSDL_Sem; lSDL;
procedure SDL_DestroySemaphore(sem: PSDL_Sem); lSDL;
function SDL_SemWait(sem: PSDL_Sem): longint; lSDL;
function SDL_SemTryWait(sem: PSDL_Sem): longint; lSDL;
function SDL_SemWaitTimeout(sem: PSDL_Sem; ms: Uint32): longint; lSDL;
function SDL_SemPost(sem: PSDL_Sem): longint; lSDL;
function SDL_SemValue(sem: PSDL_Sem): Uint32; lSDL;

function SDL_CreateCond: PSDL_Cond; lSDL;
procedure SDL_DestroyCond(cond: PSDL_Cond); lSDL;
function SDL_CondSignal(cond: PSDL_Cond): longint; lSDL;
function SDL_CondBroadcast(cond: PSDL_Cond): longint; lSDL;
function SDL_CondWait(cond: PSDL_Cond; mutex: PSDL_Mutex): longint; lSDL;
function SDL_CondWaitTimeout(cond: PSDL_Cond; mutex: PSDL_Mutex;
                             ms: Uint32): longint; lSDL;

//=====SDL_THREAD=====

type
  PSDL_Thread=pointer;

  TSDL_threadID=longword;
  TSDL_TLSID=longword;

  TSDL_ThreadPriority=(
    SDL_THREAD_PRIORITY_LOW,
    SDL_THREAD_PRIORITY_NORMAL,
    SDL_THREAD_PRIORITY_HIGH);

  TSDL_ThreadFunction=function(data: pointer): longint; cdecl;

function SDL_CreateThread(fn: TSDL_ThreadFunction;
                          name: pchar; data: pointer): PSDL_Thread; lSDL;
function SDL_GetThreadName(thread: PSDL_Thread): pchar; lSDL;
function SDL_ThreadID: TSDL_threadID; lSDL;
function SDL_GetThreadID(thread: PSDL_Thread): TSDL_threadID; lSDL;
function SDL_SetThreadPriority(priority: TSDL_ThreadPriority): longint; lSDL;
procedure SDL_WaitThread(thread: PSDL_Thread; status: plongint); lSDL;
procedure SDL_DetachThread(thread: PSDL_Thread); lSDL;

function SDL_TLSCreate: TSDL_TLSID; lSDL;
function SDL_TLSGet(id: TSDL_TLSID): pointer; lSDL;
function SDL_TLSSet(id: TSDL_TLSID; const value: pointer;
                    destructor_: pointer): longint; lSDL;

//=====SDL_RWOPS=====

const
  SDL_RWOPS_UNKNOWN  =0;
  SDL_RWOPS_WINFILE  =1;
  SDL_RWOPS_STDFILE  =2;
  SDL_RWOPS_JNIFILE  =3;
  SDL_RWOPS_MEMORY   =4;
  SDL_RWOPS_MEMORY_RO=5;

  RW_SEEK_SET=0;
  RW_SEEK_CUR=1;
  RW_SEEK_END=2;

type
  PSDL_RWops=^TSDL_RWops;
  TSDL_RWops=record
    size: function(context: PSDL_RWops): Sint64; cdecl;
    seek: function(context: PSDL_RWops; offset: Sint64;
                   whence: longint): Sint64; cdecl;
    read: function(context: PSDL_RWops; ptr: pointer;
                   size, maxnum: size_t): size_t; cdecl;
    write: function(context: PSDL_RWops; const ptr: pointer;
                    size, num: size_t): size_t; cdecl;
    close: function(context: PSDL_RWops): longint; cdecl;
    type_: Uint32;
    hidden: record
      case Uint32 of
        {$IFDEF ANDROID}
        0: (androidio: record
              fileNameRef,
              inputStreamRef,
              readableByteChannelRef,
              readMethod,
              assetFileDescriptorRef: pointer;
              position,
              size,
              offset,
              fd: longint;
            end;);
        {$ENDIF}
        {$IFDEF WINDOWS}
        0: (windowsio: record
              append: SDL_bool;
              h: pointer;
              buffer: record
                data: pointer;
                size,
                left: size_t;
              end;
            end;);
        {$ENDIF}
        {$IFDEF HAVE_STDIO_H}
        1: (stdio: record
            autoclose: SDL_bool;
            fp: pointer;
          end;);
        {$ENDIF}
        2: (mem: record
            base,
            here,
            stop: PUint8;
          end;);
        3: (unknown: record
            data1,
            data2: pointer;
          end;);
    end;
  end;

function SDL_RWFromFile(const file_: pchar;
                        const mode: pchar): PSDL_RWops; lSDL;
function SDL_RWFromFP(fp: pointer; autoclose: SDL_bool): PSDL_RWops; lSDL;
function SDL_RWFromMem(mem: pointer; size: longint): PSDL_RWops; lSDL;
function SDL_RWFromConstMem(const mem: pointer;
                            size: longint): PSDL_RWops; lSDL;

function SDL_AllocRW: PSDL_RWops; lSDL;
procedure SDL_FreeRW(area: PSDL_RWops); lSDL;

function SDL_RWsize(ctx: PSDL_RWops): Sint64; inline;
function SDL_RWseek(ctx: PSDL_RWops; offset: Sint64;
                    whence: longint): Sint64; inline;
function SDL_RWtell(ctx: PSDL_RWops): Sint64; inline;
function SDL_RWread(ctx: PSDL_RWops; ptr: pointer;
                    size ,n: longword): longword; inline;
function SDL_RWwrite(ctx: PSDL_RWops; ptr: pointer;
                     size, n: longword): longword; inline;
function SDL_RWclose(ctx: PSDL_RWops): longint; inline;

function SDL_ReadU8(src: PSDL_RWops): Uint8; lSDL;
function SDL_ReadLE16(src: PSDL_RWops): Uint16; lSDL;
function SDL_ReadBE16(src: PSDL_RWops): Uint16; lSDL;
function SDL_ReadLE32(src: PSDL_RWops): Uint32; lSDL;
function SDL_ReadBE32(src: PSDL_RWops): Uint32; lSDL;
function SDL_ReadLE64(src: PSDL_RWops): Uint64; lSDL;
function SDL_ReadBE64(src: PSDL_RWops): Uint64; lSDL;

function SDL_WriteU8(dst: PSDL_RWops; value: Uint8): size_t; lSDL;
function SDL_WriteLE16(dst: PSDL_RWops; value: Uint16): size_t; lSDL;
function SDL_WriteBE16(dst: PSDL_RWops; value: Uint16): size_t; lSDL;
function SDL_WriteLE32(dst: PSDL_RWops; value: Uint32): size_t; lSDL;
function SDL_WriteBE32(dst: PSDL_RWops; value: Uint32): size_t; lSDL;
function SDL_WriteLE64(dst: PSDL_RWops; value: Uint64): size_t; lSDL;
function SDL_WriteBE64(dst: PSDL_RWops; value: Uint64): size_t; lSDL;

//=====SDL_AUDIO=====

const
  AUDIO_U8    =$0008;
  AUDIO_S8    =$8008;
  AUDIO_U16LSB=$0010;
  AUDIO_S16LSB=$8010;
  AUDIO_U16MSB=$1010;
  AUDIO_S16MSB=$9010;
  AUDIO_U16   =AUDIO_U16LSB;
  AUDIO_S16   =AUDIO_S16LSB;

  AUDIO_S32LSB=$8020;
  AUDIO_S32MSB=$9020;
  AUDIO_S32   =AUDIO_S32LSB;

  AUDIO_F32LSB=$8120;
  AUDIO_F32MSB=$9120;
  AUDIO_F32   =AUDIO_F32LSB;

{$IFDEF FPC_LITTLE_ENDIAN}
  AUDIO_U16SYS=AUDIO_U16LSB;
  AUDIO_S16SYS=AUDIO_S16LSB;
  AUDIO_S32SYS=AUDIO_S32LSB;
  AUDIO_F32SYS=AUDIO_F32LSB;
{$ELSE}
  AUDIO_U16SYS=AUDIO_U16MSB;
  AUDIO_S16SYS=AUDIO_S16MSB;
  AUDIO_S32SYS=AUDIO_S32MSB;
  AUDIO_F32SYS=AUDIO_F32MSB;
{$ENDIF}

  SDL_AUDIO_ALLOW_FREQUENCY_CHANGE=$00000001;
  SDL_AUDIO_ALLOW_FORMAT_CHANGE   =$00000002;
  SDL_AUDIO_ALLOW_CHANNELS_CHANGE =$00000004;
  SDL_AUDIO_ALLOW_ANY_CHANGE=SDL_AUDIO_ALLOW_FREQUENCY_CHANGE or
              SDL_AUDIO_ALLOW_FORMAT_CHANGE or SDL_AUDIO_ALLOW_CHANNELS_CHANGE;

  SDL_MIX_MAXVOLUME=128;

type
  TSDL_AudioFormat=Uint16;

  TSDL_AudioCallback=procedure(userdata: pointer; stream: PUint8;
                               len: longint); cdecl;
  PSDL_AudioSpec=^TSDL_AudioSpec;
  TSDL_AudioSpec=record
    freq: longint;
    format: TSDL_AudioFormat;
    channels,
    silence: Uint8;
    samples,
    padding: Uint16;
    size: Uint32;
    callback: TSDL_AudioCallback;
    userdata: pointer;
  end;

  PSDL_AudioCVT=^TSDL_AudioCVT;

  TSDL_AudioFilter=procedure(cvt: PSDL_AudioCVT;
                             format: TSDL_AudioFormat); cdecl;

  TSDL_AudioCVT=record
    needed: longint;
    src_format,
    dst_format: TSDL_AudioFormat;
    rate_incr: double;
    buf: PUint8;
    len,
    len_cvt,
    len_mult: longint;
    len_ratio: double;
    filters: array[0..9] of TSDL_AudioFilter;
    filter_index: longint;
  end;

  TSDL_AudioDeviceID=Uint32;

  TSDL_AudioStatus=(
    SDL_AUDIO_STOPPED=0,
    SDL_AUDIO_PLAYING,
    SDL_AUDIO_PAUSED);

const
  SDL_AUDIO_MASK_BITSIZE=$FF;
  SDL_AUDIO_MASK_DATATYPE=1 shl 8;
  SDL_AUDIO_MASK_ENDIAN=1 shl 12;
  SDL_AUDIO_MASK_SIGNED=1 shl 15;

function SDL_AUDIO_BITSIZE(x: Uint16): Uint16; inline;
function SDL_AUDIO_ISFLOAT(x: Uint16): boolean; inline;
function SDL_AUDIO_ISBIGENDIAN(x: Uint16): boolean; inline;
function SDL_AUDIO_ISSIGNED(x: Uint16): boolean; inline;
function SDL_AUDIO_ISINT(x: Uint16): boolean; inline;
function SDL_AUDIO_ISLITTLEENDIAN(x: Uint16): boolean; inline;
function SDL_AUDIO_ISUNSIGNED(x: Uint16): boolean; inline;

function SDL_GetNumAudioDrivers: longint; lSDL;
function SDL_GetAudioDriver(index: longint): pchar; lSDL;
function SDL_AudioInit(driver_name: pchar): longint; lSDL;
procedure SDL_SDL_AudioQuit; lSDL;
function SDL_GetCurrentAudioDriver: pchar; lSDL;
function SDL_OpenAudio(desired, obtained: PSDL_AudioSpec): longint; lSDL;
function SDL_GetNumAudioDevices(iscapture: longint): longint; lSDL;
function SDL_GetAudioDeviceName(index, iscapture: longint): pchar; lSDL;
function SDL_OpenAudioDevice(device: pchar; iscapture: longint;
                            desired, obtained: PSDL_AudioSpec;
                            allowed_changes: longint): TSDL_AudioDeviceID; lSDL;
function SDL_GetAudioStatus: TSDL_AudioStatus; lSDL;
function SDL_GetAudioDeviceStatus(dev: TSDL_AudioDeviceID): TSDL_AudioStatus; lSDL;
procedure SDL_PauseAudio(pause_on: longint); lSDL;
procedure SDL_PauseAudioDevice(dev: TSDL_AudioDeviceID; pause_on: longint); lSDL;
function SDL_LoadWAV(file_: pchar; spec: PSDL_AudioSpec;
                 audio_buf: ppbyte; audio_len: PUint32): PSDL_AudioSpec; inline;
function SDL_LoadWAV_RW(src: PSDL_RWops; freesrc: longint; spec: PSDL_AudioSpec;
                   audio_buf: ppbyte; audio_len: PUint32): PSDL_AudioSpec; lSDL;
procedure SDL_FreeWAV(audio_buf: PUint8); lSDL;
function SDL_BuildAudioCVT(cvt: PSDL_AudioCVT; src_format: TSDL_AudioFormat;
           src_channels: UInt8; src_rate: longint; dst_format: TSDL_AudioFormat;
           dst_channels: UInt8; dst_rate: longint): longint; lSDL;
function SDL_ConvertAudio(cvt: PSDL_AudioCVT): longint; lSDL;
procedure SDL_MixAudio(dst: PUint8; const src: PUint8;
                       len: Uint32; volume: longint); lSDL;
procedure SDL_MixAudioFormat(dst: PUint8; const src: PUint8;
                  format: TSDL_AudioFormat; len: Uint32; volume: longint); lSDL;
procedure SDL_LockAudio; lSDL;
procedure SDL_LockAudioDevice(dev: TSDL_AudioDeviceID); lSDL;
procedure SDL_UnlockAudio; lSDL;
procedure SDL_UnlockAudioDevice(dev: TSDL_AudioDeviceID); lSDL;
procedure SDL_CloseAudio; lSDL;
procedure SDL_CloseAudioDevice(dev: TSDL_AudioDeviceID); lSDL;

//=====SDL_PIXELS=====

const
  SDL_ALPHA_OPAQUE=255;
  SDL_ALPHA_TRANSPARENT=0;

  SDL_PIXELTYPE_UNKNOWN=0;
  SDL_PIXELTYPE_INDEX1=1;
  SDL_PIXELTYPE_INDEX4=2;
  SDL_PIXELTYPE_INDEX8=3;
  SDL_PIXELTYPE_PACKED8=4;
  SDL_PIXELTYPE_PACKED16=5;
  SDL_PIXELTYPE_PACKED32=6;
  SDL_PIXELTYPE_ARRAYU8=7;
  SDL_PIXELTYPE_ARRAYU16=8;
  SDL_PIXELTYPE_ARRAYU32=9;
  SDL_PIXELTYPE_ARRAYF16=10;
  SDL_PIXELTYPE_ARRAYF32=11;

  SDL_BITMAPORDER_NONE=0;
  SDL_BITMAPORDER_4321=1;
  SDL_BITMAPORDER_1234=2;

  SDL_PACKEDORDER_NONE=0;
  SDL_PACKEDORDER_XRGB=1;
  SDL_PACKEDORDER_RGBX=2;
  SDL_PACKEDORDER_ARGB=3;
  SDL_PACKEDORDER_RGBA=4;
  SDL_PACKEDORDER_XBGR=5;
  SDL_PACKEDORDER_BGRX=6;
  SDL_PACKEDORDER_ABGR=7;
  SDL_PACKEDORDER_BGRA=8;

  SDL_ARRAYORDER_NONE=0;
  SDL_ARRAYORDER_RGB=1;
  SDL_ARRAYORDER_RGBA=2;
  SDL_ARRAYORDER_ARGB=3;
  SDL_ARRAYORDER_BGR=4;
  SDL_ARRAYORDER_BGRA=5;
  SDL_ARRAYORDER_ABGR=6;

  SDL_PACKEDLAYOUT_NONE=0;
  SDL_PACKEDLAYOUT_332=1;
  SDL_PACKEDLAYOUT_4444=2;
  SDL_PACKEDLAYOUT_1555=3;
  SDL_PACKEDLAYOUT_5551=4;
  SDL_PACKEDLAYOUT_565=5;
  SDL_PACKEDLAYOUT_8888=6;
  SDL_PACKEDLAYOUT_2101010=7;
  SDL_PACKEDLAYOUT_1010102=8;

  SDL_PIXELFORMAT_UNKNOWN  =0;
  SDL_PIXELFORMAT_INDEX1LSB=$11100100;
  SDL_PIXELFORMAT_INDEX1MSB=$11200100;
  SDL_PIXELFORMAT_INDEX4LSB=$12100400;
  SDL_PIXELFORMAT_INDEX4MSB=$12200400;
  SDL_PIXELFORMAT_INDEX8   =$13000801;
  SDL_PIXELFORMAT_RGB332   =$14110801;
  SDL_PIXELFORMAT_RGB444   =$15120C02;
  SDL_PIXELFORMAT_RGB555   =$15130F02;
  SDL_PIXELFORMAT_BGR555   =$15530F02;
  SDL_PIXELFORMAT_ARGB4444 =$15321002;
  SDL_PIXELFORMAT_RGBA4444 =$15421002;
  SDL_PIXELFORMAT_ABGR4444 =$15721002;
  SDL_PIXELFORMAT_BGRA4444 =$15821002;
  SDL_PIXELFORMAT_ARGB1555 =$15331002;
  SDL_PIXELFORMAT_RGBA5551 =$15441002;
  SDL_PIXELFORMAT_ABGR1555 =$15731002;
  SDL_PIXELFORMAT_BGRA5551 =$15841002;
  SDL_PIXELFORMAT_RGB565   =$15151002;
  SDL_PIXELFORMAT_BGR565   =$15551002;
  SDL_PIXELFORMAT_RGB24    =$17101803;
  SDL_PIXELFORMAT_BGR24    =$17401803;
  SDL_PIXELFORMAT_RGB888   =$16161804;
  SDL_PIXELFORMAT_RGBX8888 =$16261804;
  SDL_PIXELFORMAT_BGR888   =$16561804;
  SDL_PIXELFORMAT_BGRX8888 =$16661804;
  SDL_PIXELFORMAT_ARGB8888 =$16362004;
  SDL_PIXELFORMAT_RGBA8888 =$16462004;
  SDL_PIXELFORMAT_ABGR8888 =$16762004;
  SDL_PIXELFORMAT_BGRA8888 =$16862004;
  SDL_PIXELFORMAT_ARGB2101010=$16372004;

  SDL_PIXELFORMAT_YV12=$32315659;
  SDL_PIXELFORMAT_IYUV=$56555949;
  SDL_PIXELFORMAT_YUY2=$32595559;
  SDL_PIXELFORMAT_UYVY=$59565955;
  SDL_PIXELFORMAT_YVYU=$55595659;

type
  PSDL_Color=^TSDL_Color;
  TSDL_Color=record
    r, g, b, a: Uint8;
  end;

  TSDL_Colour=TSDL_Color;
  PSDL_Colour=^TSDL_Colour;

  PSDL_Palette=^TSDL_Palette;
  TSDL_Palette=record
    ncolors: longint;
    colors: PSDL_Color;
    version: Uint32;
    refcount: longint;
  end;

  PSDL_PixelFormat=^TSDL_PixelFormat;
  TSDL_PixelFormat=record
    format: Uint32;
    palette: PSDL_Palette;
    BitsPerPixel,
    BytesPerPixel: Uint8;
    padding: array[0..1] of Uint8;
    Rmask,
    Gmask,
    Bmask,
    Amask: Uint32;
    Rloss,
    Gloss,
    Bloss,
    Aloss: Uint8;
    Rshift,
    Gshift,
    Bshift,
    Ashift: Uint8;
    refcount: longint;
    next: PSDL_PixelFormat;
  end;

function SDL_PIXELFLAG(X: Uint32): Uint32; inline;
function SDL_PIXELTYPE(X: Uint32): Uint32; inline;
function SDL_PIXELORDER(X: Uint32): Uint32; inline;
function SDL_PIXELLAYOUT(X: Uint32): Uint32; inline;
function SDL_BITSPERPIXEL(X: Uint32): Uint32; inline;
function SDL_BYTESPERPIXEL(X: Uint32): Uint32; inline;
function SDL_ISPIXELFORMAT_INDEXED(format: Uint32): boolean; inline;
function SDL_ISPIXELFORMAT_ALPHA(format: Uint32): boolean; inline;
function SDL_ISPIXELFORMAT_FOURCC(format: Uint32): boolean; inline;

function SDL_GetPixelFormatName(format: Uint32): pchar; lSDL;
function SDL_PixelFormatEnumToMasks(format: Uint32; bpp: plongint;
                           Rmask, Gmask, Bmask, Amask: PUint32): SDL_bool; lSDL;
function SDL_MasksToPixelFormatEnum(bpp: longint;
                           Rmask, Gmask, Bmask, Amask: Uint32): Uint32; lSDL;
function SDL_AllocFormat(pixel_format: Uint32): PSDL_PixelFormat; lSDL;
procedure SDL_FreeFormat(format: PSDL_PixelFormat); lSDL;
function SDL_AllocPalette(ncolors: longint): PSDL_Palette; lSDL;
function SDL_SetPixelFormatPalette(format: PSDL_PixelFormat;
                                   palette: PSDL_Palette): longint; lSDL;
function SDL_SetPaletteColors(palette: PSDL_Palette; const colors: PSDL_Color;
                              firstcolor, ncolors: longint): longint; lSDL;
procedure SDL_FreePalette(palette: PSDL_Palette); lSDL;
function SDL_MapRGB(const format: PSDL_PixelFormat;
                    r, g, b: Uint8): Uint32; lSDL;
function SDL_MapRGBA(const format: PSDL_PixelFormat;
                     r, g, b, a: Uint8): Uint32; lSDL;
procedure SDL_GetRGB(pixel: Uint32; const format: PSDL_PixelFormat;
                     r, g, b: PUint8); lSDL;
procedure SDL_GetRGBA(pixel: Uint32; const format: PSDL_PixelFormat;
                      r, g, b, a: PUint8); lSDL;
procedure SDL_CalculateGammaRamp(gamma: single; ramp: PUint16); lSDL;

//=====SDL_RECT=====

type
  PSDL_Point=^TSDL_Point;
  TSDL_Point=record
    x, y: longint;
  end;

  PSDL_Rect=^TSDL_Rect;
  TSDL_Rect=record
    x, y,
    w, h: longint;
  end;

function SDL_RectEmpty(const x: PSDL_Rect): SDL_bool; inline;
function SDL_RectEquals(const a, b: PSDL_Rect): SDL_bool; inline;

function SDL_HasIntersection(const a, b: PSDL_Rect): SDL_bool; lSDL;
function SDL_IntersectRect(const a, b: PSDL_Rect;
                           result: PSDL_Rect): SDL_bool; lSDL;
procedure SDL_UnionRect(const a, b: PSDL_Rect; result: PSDL_Rect); lSDL;
function SDL_EnclosePoints(const points: PSDL_Point; count: longint;
                      const clip: PSDL_Rect; result: PSDL_Rect): SDL_bool; lSDL;
function SDL_IntersectRectAndLine(const rect: PSDL_Rect;
                                  x1, y1, x2, y2: plongint): SDL_bool; lSDL;

//=====SDL_BLENDMODE=====

const
  SDL_BLENDMODE_NONE =$00000000;
  SDL_BLENDMODE_BLEND=$00000001;
  SDL_BLENDMODE_ADD  =$00000002;
  SDL_BLENDMODE_MOD  =$00000004;

//=====SDL_SURFACE=====

const
  SDL_SWSURFACE=0;
  SDL_PREALLOC =$00000001;
  SDL_RLEACCEL =$00000002;
  SDL_DONTFREE =$00000004;

type
  PSDL_Surface=^TSDL_Surface;
  TSDL_Surface=record
    flags: Uint32;
    format: PSDL_PixelFormat;
    w, h,
    pitch: longint;
    pixels,
    userdata: pointer;
    locked: longint;
    lock_data: pointer;
    clip_rect: TSDL_Rect;
    map: pointer;
    refcount: longint;
  end;

  TSDL_Blit=function(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface;
                     dstrect: PSDL_Rect): longint;

function SDL_MUSTLOCK(S: PSDL_Surface): boolean; inline;

function SDL_CreateRGBSurface(flags: Uint32;width, height, depth: longint;
                        Rmask, Gmask, Bmask, Amask: Uint32): PSDL_Surface; lSDL;
function SDL_CreateRGBSurfaceFrom(pixels: pointer; width, height, depth,
        pitch: longint; Rmask, Gmask, Bmask, Amask: Uint32): PSDL_Surface; lSDL;
procedure SDL_FreeSurface(surface: PSDL_Surface); lSDL;

function SDL_SetSurfacePalette(surface: PSDL_Surface;
                               palette: PSDL_Palette): longint; lSDL;
function SDL_LockSurface(surface: PSDL_Surface): longint; lSDL;
procedure SDL_UnlockSurface(surface: PSDL_Surface); lSDL;
function SDL_LoadBMP_RW(src: PSDL_RWops; freesrc: longint): PSDL_Surface; lSDL;
function SDL_LoadBMP(file_: pchar): PSDL_Surface; inline;
function SDL_SaveBMP_RW(surface: PSDL_Surface; dst: PSDL_RWops;
                        freedst: longint): longint; lSDL;
function SDL_SaveBMP(surface: PSDL_Surface; file_: pchar): longint; inline;
function SDL_SetSurfaceRLE(surface: PSDL_Surface; flag: longint): longint; lSDL;
function SDL_SetColorKey(surface: PSDL_Surface; flag: longint;
                         key: Uint32): longint; lSDL;
function SDL_GetColorKey(surface: PSDL_Surface; key: PUint32): longint; lSDL;
function SDL_SetSurfaceColorMod(surface: PSDL_Surface;
                                r, g, b: Uint8): longint; lSDL;
function SDL_GetSurfaceColorMod(surface: PSDL_Surface;
                                r, g, b: PUint8): longint; lSDL;
function SDL_SetSurfaceAlphaMod(surface: PSDL_Surface;
                                alpha: Uint8): longint; lSDL;
function SDL_GetSurfaceAlphaMod(surface: PSDL_Surface;
                                alpha: PUint8): longint; lSDL;
function SDL_SetSurfaceBlendMode(surface: PSDL_Surface;
                                 blendMode: longword): longint; lSDL;
function SDL_GetSurfaceBlendMode(surface: PSDL_Surface;
                                 blendMode: plongword): longint; lSDL;
function SDL_SetClipRect(surface: PSDL_Surface;
                         const rect: PSDL_Rect): SDL_bool; lSDL;
procedure SDL_GetClipRect(surface: PSDL_Surface; rect: PSDL_Rect); lSDL;
function SDL_ConvertSurface(src: PSDL_Surface; fmt: PSDL_PixelFormat;
                            flags: Uint32): PSDL_Surface; lSDL;
function SDL_ConvertSurfaceFormat(src: PSDL_Surface; pixel_format: Uint32;
                                  flags: Uint32): PSDL_Surface; lSDL;
function SDL_ConvertPixels(width, height: longint;
           src_format: Uint32; const src: pointer; src_pitch: longint;
           dst_format: Uint32; dst: pointer; dst_pitch: longint): longint; lSDL;
function SDL_FillRect(dst: PSDL_Surface; const rect: PSDL_Rect;
                      color: Uint32): longint; lSDL;
function SDL_FillRects(dst: PSDL_Surface; const rects: PSDL_Rect;
                       count: longint; color: Uint32): longint; lSDL;
function SDL_BlitSurface(src: PSDL_Surface; const srcrect: PSDL_Rect;
                        dst: PSDL_Surface; dstrect: PSDL_Rect): longint; inline;
function SDL_UpperBlit(src: PSDL_Surface; const srcrect: PSDL_Rect;
                       dst: PSDL_Surface; dstrect: PSDL_Rect): longint; lSDL;
function SDL_LowerBlit(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface;
                       dstrect: PSDL_Rect): longint; lSDL;
function SDL_SoftStretch(src: PSDL_Surface; const srcrect: PSDL_Rect;
                 dst: PSDL_Surface; const dstrect: PSDL_Surface): longint; lSDL;
function SDL_BlitScaled(src: PSDL_Surface; const srcrect: PSDL_Rect;
                        dst: PSDL_Surface; dstrect: PSDL_Rect): longint; inline;
function SDL_UpperBlitScaled(src: PSDL_Surface; const srcrect: PSDL_Rect;
                          dst: PSDL_Surface; dstrect: PSDL_Rect): longint; lSDL;
function SDL_LowerBlitScaled(src: PSDL_Surface; srcrect: PSDL_Rect;
                          dst: PSDL_Surface; dstrect: PSDL_Rect): longint; lSDL;

//=====SDL_VIDEO=====

const
  //WindowFlags
  SDL_WINDOW_FULLSCREEN=$00000001;
  SDL_WINDOW_OPENGL=$00000002;
  SDL_WINDOW_SHOWN=$00000004;
  SDL_WINDOW_HIDDEN=$00000008;
  SDL_WINDOW_BORDERLESS=$00000010;
  SDL_WINDOW_RESIZABLE=$00000020;
  SDL_WINDOW_MINIMIZED=$00000040;
  SDL_WINDOW_MAXIMIZED=$00000080;
  SDL_WINDOW_INPUT_GRABBED=$00000100;
  SDL_WINDOW_INPUT_FOCUS=$00000200;
  SDL_WINDOW_MOUSE_FOCUS=$00000400;
  SDL_WINDOW_FULLSCREEN_DESKTOP=SDL_WINDOW_FULLSCREEN or $00001000;
  SDL_WINDOW_FOREIGN=$00000800;
  SDL_WINDOW_ALLOW_HIGHDPI=$00002000;

  SDL_WINDOWPOS_UNDEFINED_MASK=$1FFF0000;
  SDL_WINDOWPOS_UNDEFINED=SDL_WINDOWPOS_UNDEFINED_MASK or 0;
  SDL_WINDOWPOS_CENTERED_MASK=$2FFF0000;
  SDL_WINDOWPOS_CENTERED=SDL_WINDOWPOS_CENTERED_MASK or 0;
  //WindowEventID
  SDL_WINDOWEVENT_NONE=0;
  SDL_WINDOWEVENT_SHOWN=1;
  SDL_WINDOWEVENT_HIDDEN=2;
  SDL_WINDOWEVENT_EXPOSED=3;
  SDL_WINDOWEVENT_MOVED=4;
  SDL_WINDOWEVENT_RESIZED=5;
  SDL_WINDOWEVENT_SIZE_CHANGED=6;
  SDL_WINDOWEVENT_MINIMIZED=7;
  SDL_WINDOWEVENT_MAXIMIZED=8;
  SDL_WINDOWEVENT_RESTORED=9;
  SDL_WINDOWEVENT_ENTER=10;
  SDL_WINDOWEVENT_LEAVE=11;
  SDL_WINDOWEVENT_FOCUS_GAINED=12;
  SDL_WINDOWEVENT_FOCUS_LOST=13;
  SDL_WINDOWEVENT_CLOSE=14;
  //GLprofile
  SDL_GL_CONTEXT_PROFILE_CORE         =$0001;
  SDL_GL_CONTEXT_PROFILE_COMPATIBILITY=$0002;
  SDL_GL_CONTEXT_PROFILE_ES           =$0004;
  //GLcontextFlag
  SDL_GL_CONTEXT_DEBUG_FLAG             =$0001;
  SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG=$0002;
  SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG     =$0004;
  SDL_GL_CONTEXT_RESET_ISOLATION_FLAG   =$0008;

type
  PSDL_DisplayMode=^TSDL_DisplayMode;
  TSDL_DisplayMode=record
    format: Uint32;
    w, h,
    refresh_rate: longint;
    driverdata: pointer;
  end;

  PPSDL_Window=^PSDL_Window;
  PSDL_Window=pointer;

  SDL_GLContext=pointer;

  SDL_GLattr=(
    SDL_GL_RED_SIZE,
    SDL_GL_GREEN_SIZE,
    SDL_GL_BLUE_SIZE,
    SDL_GL_ALPHA_SIZE,
    SDL_GL_BUFFER_SIZE,
    SDL_GL_DOUBLEBUFFER,
    SDL_GL_DEPTH_SIZE,
    SDL_GL_STENCIL_SIZE,
    SDL_GL_ACCUM_RED_SIZE,
    SDL_GL_ACCUM_GREEN_SIZE,
    SDL_GL_ACCUM_BLUE_SIZE,
    SDL_GL_ACCUM_ALPHA_SIZE,
    SDL_GL_STEREO,
    SDL_GL_MULTISAMPLEBUFFERS,
    SDL_GL_MULTISAMPLESAMPLES,
    SDL_GL_ACCELERATED_VISUAL,
    SDL_GL_RETAINED_BACKING,
    SDL_GL_CONTEXT_MAJOR_VERSION,
    SDL_GL_CONTEXT_MINOR_VERSION,
    SDL_GL_CONTEXT_EGL,
    SDL_GL_CONTEXT_FLAGS,
    SDL_GL_CONTEXT_PROFILE_MASK,
    SDL_GL_SHARE_WITH_CURRENT_CONTEXT,
    SDL_GL_FRAMEBUFFER_SRGB_CAPABLE);

function SDL_GetNumVideoDrivers: longint; lSDL;
function SDL_GetVideoDriver(index: longint): pchar; lSDL;
function SDL_VideoInit(const driver_name: pchar): longint; lSDL;
procedure SDL_VideoQuit; lSDL;
function SDL_GetCurrentVideoDriver: pchar; lSDL;
function SDL_GetNumVideoDisplays: longint; lSDL;
function SDL_GetDisplayName(displayIndex: longint): pchar; lSDL;
function SDL_GetDisplayBounds(displayIndex: longint;
                              rect: PSDL_Rect): longint; lSDL;
function SDL_GetNumDisplayModes(displayIndex: longint): longint; lSDL;
function SDL_GetDisplayMode(displayIndex, modeIndex: longint;
                            mode: PSDL_DisplayMode): longint; lSDL;
function SDL_GetDesktopDisplayMode(displayIndex: longint;
                                   mode: PSDL_DisplayMode): longint; lSDL;
function SDL_GetCurrentDisplayMode(displayIndex: longint;
                                   mode: PSDL_DisplayMode): longint; lSDL;
function SDL_GetClosestDisplayMode(displayIndex: longint;
                 const mode, closest: PSDL_DisplayMode): PSDL_DisplayMode; lSDL;
function SDL_GetWindowDisplayIndex(window: PSDL_Window): longint; lSDL;
function SDL_SetWindowDisplayMode(window: PSDL_Window;
                                  const mode: PSDL_DisplayMode): longint; lSDL;
function SDL_GetWindowDisplayMode(window: PSDL_Window;
                                  mode: PSDL_DisplayMode): longint; lSDL;
function SDL_GetWindowPixelFormat(window: PSDL_Window): Uint32; lSDL;

function SDL_CreateWindow(const title: pchar; x, y, w, h: longint;
                          flags: Uint32): PSDL_Window; lSDL;
function SDL_CreateWindowFrom(const data: pointer): PSDL_Window; lSDL;
function SDL_GetWindowID(window: PSDL_Window): Uint32; lSDL;
function SDL_GetWindowFromID(id: Uint32): PSDL_Window; lSDL;
function SDL_GetWindowFlags(window: PSDL_Window): Uint32; lSDL;
procedure SDL_SetWindowTitle(window: PSDL_Window; const title: pchar); lSDL;
function SDL_GetWindowTitle(window: PSDL_Window): pchar; lSDL;
procedure SDL_SetWindowIcon(window: PSDL_Window; icon: PSDL_Surface); lSDL;
function SDL_SetWindowData(window: PSDL_Window; const name: pchar;
                           userdata: pointer): pointer; lSDL;
function SDL_GetWindowData(window: PSDL_Window;
                           const name: pchar): pointer; lSDL;
procedure SDL_SetWindowPosition(window: PSDL_Window; x, y: longint); lSDL;
procedure SDL_GetWindowPosition(window: PSDL_Window; x, y: plongint); lSDL;
procedure SDL_SetWindowSize(window: PSDL_Window; w, h: longint); lSDL;
procedure SDL_GetWindowSize(window: PSDL_Window; w, h: plongint); lSDL;
procedure SDL_SetWindowMinimumSize(window: PSDL_Window;
                                   min_w, min_h: longint); lSDL;
procedure SDL_GetWindowMinimumSize(window: PSDL_Window; w, h: plongint); lSDL;
procedure SDL_SetWindowMaximumSize(window: PSDL_Window;
                                   max_w, max_h: longint); lSDL;
procedure SDL_GetWindowMaximumSize(window: PSDL_Window; w, h: plongint); lSDL;

procedure SDL_SetWindowBordered(window: PSDL_Window; bordered: SDL_bool); lSDL;
procedure SDL_ShowWindow(window: PSDL_Window); lSDL;
procedure SDL_HideWindow(window: PSDL_Window); lSDL;
procedure SDL_RaiseWindow(window: PSDL_Window); lSDL;
procedure SDL_MaximizeWindow(window: PSDL_Window); lSDL;
procedure SDL_MinimizeWindow(window: PSDL_Window); lSDL;
procedure SDL_RestoreWindow(window: PSDL_Window); lSDL;
function SDL_SetWindowFullscreen(window: PSDL_Window;
                                 flags: Uint32): longint; lSDL;
function SDL_GetWindowSurface(window: PSDL_Window): PSDL_Surface; lSDL;
function SDL_UpdateWindowSurface(window: PSDL_Window): longint; lSDL;
function SDL_UpdateWindowSurfaceRects(window: PSDL_Window; rects: PSDL_Rect;
                                      numrects: longint): longint; lSDL;
procedure SDL_SetWindowGrab(window: PSDL_Window; grabbed: SDL_bool); lSDL;
function SDL_GetWindowGrab(window: PSDL_Window): SDL_bool; lSDL;
function SDL_SetWindowBrightness(window: PSDL_Window;
                                 brightness: single): longint; lSDL;
function SDL_GetWindowBrightness(window: PSDL_Window): single; lSDL;
function SDL_SetWindowGammaRamp(window: PSDL_Window;
                                const red, green, blue: PUint16): longint; lSDL;
function SDL_GetWindowGammaRamp(window: PSDL_Window;
                                red, green, blue: PUint16): longint; lSDL;

procedure SDL_DestroyWindow(window: PSDL_Window); lSDL;
function SDL_IsScreenSaverEnabled: SDL_bool; lSDL;
procedure SDL_EnableScreenSaver; lSDL;
procedure SDL_DisableScreenSaver; lSDL;

function SDL_GL_LoadLibrary(const path: pchar): longint; lSDL;
function SDL_GL_GetProcAddress(const proc:  pchar): pointer; lSDL;
procedure SDL_GL_UnloadLibrary; lSDL;
function SDL_GL_ExtensionSupported(const extension: pchar): SDL_bool; lSDL;
procedure SDL_GL_ResetAttributes; lSDL;
function SDL_GL_SetAttribute(attr: SDL_GLattr; value: longint): longint; lSDL;
function SDL_GL_GetAttribute(attr: SDL_GLattr; value: plongint): longint; lSDL;
function SDL_GL_CreateContext(window: PSDL_Window): SDL_GLContext; lSDL;
function SDL_GL_MakeCurrent(window: PSDL_Window;
                            context: SDL_GLContext): longint; lSDL;
function SDL_GL_GetCurrentWindow: PSDL_Window; lSDL;
function SDL_GL_GetCurrentContext: SDL_GLContext; lSDL;
procedure SDL_GL_GetDrawableSize(window: PSDL_Window; w, h: plongint); lSDL;
function SDL_GL_SetSwapInterval(interval: longint): longint; lSDL;
function SDL_GL_GetSwapInterval: longint; lSDL;
procedure SDL_GL_SwapWindow(window: PSDL_Window); lSDL;
procedure SDL_GL_DeleteContext(context: SDL_GLContext); lSDL;

//=====SDL_SHAPE=====

const
  SDL_NONSHAPEABLE_WINDOW=-1;
  SDL_INVALID_SHAPE_ARGUMENT=-2;
  SDL_WINDOW_LACKS_SHAPE=-3;

  ShapeModeDefault=0;
  ShapeModeBinarizeAlpha=1;
  ShapeModeReverseBinarizeAlpha=2;
  ShapeModeColorKey=3;

type
  TSDL_WindowShapeParams=record
    case longint of
      0: (binarizationCutoff: Uint8;);
      1: (colorKey: TSDL_Color;);
  end;

  PSDL_WindowShapeMode=^TSDL_WindowShapeMode;
  TSDL_WindowShapeMode=record
    mode: longword;
    parameters: TSDL_WindowShapeParams;
  end;

function SDL_CreateShapedWindow(const title: pchar; x, y, w, h: longword;
                                flags: Uint32): PSDL_Window; lSDL;
function SDL_IsShapedWindow(const window: PSDL_Window): SDL_bool; lSDl;
function SDL_SetWindowShape(window: PSDL_Window; shape: PSDL_Surface;
                            shape_mode: PSDL_WindowShapeMode): longint; lSDL;
function SDL_GetShapedWindowMode(window: PSDL_Window;
                               shape_mode: PSDL_WindowShapeMode): longint; lSDL;

//=====SDL_RENDER=====

const
  SDL_RENDERER_SOFTWARE=$00000001;
  SDL_RENDERER_ACCELERATED=$00000002;
  SDL_RENDERER_PRESENTVSYNC=$00000004;
  SDL_RENDERER_TARGETTEXTURE=$00000008;

  SDL_TEXTUREACCESS_STATIC=0;
  SDL_TEXTUREACCESS_STREAMING=1;
  SDL_TEXTUREACCESS_TARGET=2;

  SDL_TEXTUREMODULATE_NONE=$00000000;
  SDL_TEXTUREMODULATE_COLOR=$00000001;
  SDL_TEXTUREMODULATE_ALPHA=$00000002;

  SDL_FLIP_NONE=$00000000;
  SDL_FLIP_HORIZONTAL=$00000001;
  SDL_FLIP_VERTICAL=$00000002;

type
  PSDL_RendererInfo=^TSDL_RendererInfo;
  TSDL_RendererInfo=record
    name: pchar;
    flags,
    num_texture_formats: Uint32;
    texture_formats: array[0..15] of Uint32;
    max_texture_width,
    max_texture_height: longint;
  end;

  PPSDL_Renderer=^PSDL_Renderer;
  PSDL_Renderer=pointer;
  PSDL_Texture=pointer;

function SDL_GetNumRenderDrivers: longint; lSDL;
function SDL_GetRenderDriverInfo(index: longint;
                                 info: PSDL_RendererInfo): longint; lSDL;
function SDL_CreateWindowAndRenderer(width, height: longint;
                                     window_flags: Uint32; window: PPSDL_Window;
                                     renderer: PPSDL_Renderer): longint; lSDL;
function SDL_CreateRenderer(window: PSDL_Window;
                            index: longint; flags: Uint32): PSDL_Renderer; lSDL;
function SDL_CreateSoftwareRenderer(surface: PSDL_Surface): PSDL_Renderer; lSDL;
function SDL_GetRenderer(window: PSDL_Window): PSDL_Renderer; lSDL;
function SDL_GetRendererInfo(renderer: PSDL_Renderer;
                             info: PSDL_RendererInfo): longint; lSDL;
function SDL_GetRendererOutputSize(renderer: PSDL_Renderer;
                                   w, h: plongint): longint; lSDL;
function SDL_CreateTexture(renderer: PSDL_Renderer; format: Uint32;
                           access, w, h: longint): PSDL_Texture; lSDL;
function SDL_CreateTextureFromSurface(renderer: PSDL_Renderer;
                                     surface: PSDL_Surface): PSDL_Texture; lSDL;
function SDL_QueryTexture(texture: PSDL_Texture; format: PUint32;
                          access, w, h: plongint): longint; lSDL;
function SDL_SetTextureColorMod(texture: PSDL_Texture;
                                r, g, b: Uint8): longint; lSDL;
function SDL_GetTextureColorMod(texture: PSDL_Texture;
                                r, g, b: PUint8): longint; lSDL;
function SDL_SetTextureAlphaMod(texture: PSDL_Texture;
                                alpha: Uint8): longint; lSDL;
function SDL_GetTextureAlphaMod(texture: PSDL_Texture;
                                alpha: PUint8): longint; lSDL;
function SDL_SetTextureBlendMode(texture: PSDL_Texture;
                                 blendMode: longword): longint; lSDL;
function SDL_GetTextureBlendMode(texture: PSDL_Texture;
                                 blendMode: plongword): longint; lSDL;
function SDL_UpdateTexture(texture: PSDL_Texture; const rect: PSDL_Rect;
                          const pixels: pointer; pitch: longint): longint; lSDL;
function SDL_UpdateYUVTexture(texture: PSDL_Texture; const rect: PSDL_Rect;
                          const Yplane: PUint8; Ypitch: longint;
                          const Uplane: PUint8; Upitch: longint;
                          const Vplane: PUint8; Vpitch: longint): longint; lSDL;
function SDL_LockTexture(texture: PSDL_Texture; const rect: PSDL_Rect;
                         pixels: ppointer; pitch: plongint): longint; lSDL;
procedure SDL_UnlockTexture(texture: PSDL_Texture); lSDL;
function SDL_RenderTargetSupported(renderer: PSDL_Renderer): SDL_bool; lSDL;
function SDL_SetRenderTarget(renderer: PSDL_Renderer;
                             texture: PSDL_Texture): longint; lSDL;
function SDL_GetRenderTarget(renderer: PSDL_Renderer): PSDL_Texture; lSDL;
function SDL_RenderSetLogicalSize(renderer: PSDL_Renderer;
                                  w, h: longint): longint; lSDL;
procedure SDL_RenderGetLogicalSize(renderer: PSDL_Renderer;
                                   w, h: plongint); lSDL;
function SDL_RenderSetViewport(renderer: PSDL_Renderer;
                               const rect: PSDL_Rect): longint; lSDL;
procedure SDL_RenderGetViewport(renderer: PSDL_Renderer; rect: PSDL_Rect); lSDL;
function SDL_RenderSetClipRect(renderer: PSDL_Renderer;
                               const rect: PSDL_Rect): longint; lSDL;
procedure SDL_RenderGetClipRect(renderer: PSDL_Renderer; rect: PSDL_Rect); lSDL;
function SDL_RenderSetScale(renderer: PSDL_Renderer;
                            scaleX, scaleY: single): longint; lSDL;
procedure SDL_RenderGetScale(renderer: PSDL_Renderer;
                             scaleX, scaleY: psingle); lSDL;

function SDL_SetRenderDrawColor(renderer: PSDL_Renderer;
                                r, g, b, a: Uint8): longint; lSDL;
function SDL_GetRenderDrawColor(renderer: PSDL_Renderer;
                                r, g, b, a: PUint8): longint; lSDL;
function SDL_SetRenderDrawBlendMode(renderer: PSDL_Renderer;
                                    blendMode: longword): longint; lSDL;
function SDL_GetRenderDrawBlendMode(renderer: PSDL_Renderer;
                                    blendMode: plongword): longint; lSDL;

function SDL_RenderClear(renderer: PSDL_Renderer): longint; lSDL;
function SDL_RenderDrawPoint(renderer: PSDL_Renderer;
                             x, y: longint): longint; lSDL;
function SDL_RenderDrawPoints(renderer: PSDL_Renderer; const points: PSDL_Point;
                              count: longint): longint; lSDL;
function SDL_RenderDrawLine(renderer: PSDL_Renderer;
                            x1, y1, x2, y2: longint): longint; lSDL;
function SDL_RenderDrawLines(renderer: PSDL_Renderer; const points: PSDL_Point;
                             count: longint): longint; lSDL;
function SDL_RenderDrawRect(renderer: PSDL_Renderer;
                            const rect: PSDL_Rect): longint; lSDL;
function SDL_RenderDrawRects(renderer: PSDL_Renderer; const rects: PSDL_Rect;
                             count: longint): longint; lSDL;
function SDL_RenderFillRect(renderer: PSDL_Renderer;
                            const rect: PSDL_Rect): longint; lSDL;
function SDL_RenderFillRects(renderer: PSDL_Renderer; const rects: PSDL_Rect;
                             count: longint): longint; lSDL;

function SDL_RenderCopy(renderer: PSDL_Renderer;
                        texture: PSDL_Texture;
                        const srcrect,
                        dstrect: PSDL_Rect): longint; lSDL;
function SDL_RenderCopyEx(renderer: PSDL_Renderer;
                          texture: PSDL_Texture;
                          const srcrect,
                          dstrect: PSDL_Rect;
                          const angle: double;
                          const center: PSDL_Point;
                          const flip: longword): longint; lSDL;
function SDL_RenderReadPixels(renderer: PSDL_Renderer;
                              const rect: PSDL_Rect;
                              format: Uint32;
                              pixels: pointer;
                              pitch: longint): longint; lSDL;
procedure SDL_RenderPresent(renderer: PSDL_Renderer); lSDL;
procedure SDL_DestroyTexture(texture: PSDL_Texture); lSDL;
procedure SDL_DestroyRenderer(renderer: PSDL_Renderer); lSDL;

function SDL_GL_BindTexture(texture: PSDL_Texture;
                            texw, texh: psingle): longint; lSDL;
function SDL_GL_UnbindTexture(texture: PSDL_Texture): longint; lSDL;

//=====SDL_MESSAGEBOX=====

const
  //MessageBoxFlags
  SDL_MESSAGEBOX_ERROR      =$00000010;
  SDL_MESSAGEBOX_WARNING    =$00000020;
  SDL_MESSAGEBOX_INFORMATION=$00000040;
  //MessageBoxButtonFlags
  SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT=$00000001;
  SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT=$00000002;
  //MessageBoxColorType
  SDL_MESSAGEBOX_COLOR_BACKGROUND=0;
  SDL_MESSAGEBOX_COLOR_TEXT=1;
  SDL_MESSAGEBOX_COLOR_BUTTON_BORDER=2;
  SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND=3;
  SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED=4;
  SDL_MESSAGEBOX_COLOR_MAX=5;

type
  PSDL_MessageBoxButtonData=^TSDL_MessageBoxButtonData;
  TSDL_MessageBoxButtonData=record
    flags: Uint32;
    buttonid: longint;
    text: pchar;
  end;

  SDL_MessageBoxColor=record
    r, g, b: Uint8;
  end;

  PSDL_MessageBoxColorScheme=^TSDL_MessageBoxColorScheme;
  TSDL_MessageBoxColorScheme=record
    colors: array[0..SDL_MESSAGEBOX_COLOR_MAX-1] of SDL_MessageBoxColor;
  end;

  PSDL_MessageBoxData=^TSDL_MessageBoxData;
  TSDL_MessageBoxData=record
    flags: Uint32;
    window: PSDL_Window;
    title,
    message: pchar;
    numbuttons: longint;
    buttons: PSDL_MessageBoxButtonData;
    colorScheme: PSDL_MessageBoxColorScheme;
  end;

function SDL_ShowMessageBox(const messageboxdata: PSDL_MessageBoxData;
                            buttonid: plongint): longint; lSDL;
function SDL_ShowSimpleMessageBox(flags: Uint32; const title, message: pchar;
                                  window: PSDL_Window): longint; lSDL;

//=====SDL_SCANCODE=====

type
  PSDL_ScanCode=^TSDL_ScanCode;
  TSDL_ScanCode=longword;

const
  SDL_SCANCODE_UNKNOWN=0;

  SDL_SCANCODE_A=4;
  SDL_SCANCODE_B=5;
  SDL_SCANCODE_C=6;
  SDL_SCANCODE_D=7;
  SDL_SCANCODE_E=8;
  SDL_SCANCODE_F=9;
  SDL_SCANCODE_G=10;
  SDL_SCANCODE_H=11;
  SDL_SCANCODE_I=12;
  SDL_SCANCODE_J=13;
  SDL_SCANCODE_K=14;
  SDL_SCANCODE_L=15;
  SDL_SCANCODE_M=16;
  SDL_SCANCODE_N=17;
  SDL_SCANCODE_O=18;
  SDL_SCANCODE_P=19;
  SDL_SCANCODE_Q=20;
  SDL_SCANCODE_R=21;
  SDL_SCANCODE_S=22;
  SDL_SCANCODE_T=23;
  SDL_SCANCODE_U=24;
  SDL_SCANCODE_V=25;
  SDL_SCANCODE_W=26;
  SDL_SCANCODE_X=27;
  SDL_SCANCODE_Y=28;
  SDL_SCANCODE_Z=29;

  SDL_SCANCODE_1=30;
  SDL_SCANCODE_2=31;
  SDL_SCANCODE_3=32;
  SDL_SCANCODE_4=33;
  SDL_SCANCODE_5=34;
  SDL_SCANCODE_6=35;
  SDL_SCANCODE_7=36;
  SDL_SCANCODE_8=37;
  SDL_SCANCODE_9=38;
  SDL_SCANCODE_0=39;

  SDL_SCANCODE_RETURN=40;
  SDL_SCANCODE_ESCAPE=41;
  SDL_SCANCODE_BACKSPACE=42;
  SDL_SCANCODE_TAB=43;
  SDL_SCANCODE_SPACE=44;

  SDL_SCANCODE_MINUS=45;
  SDL_SCANCODE_EQUALS=46;
  SDL_SCANCODE_LEFTBRACKET=47;
  SDL_SCANCODE_RIGHTBRACKET=48;
  SDL_SCANCODE_BACKSLASH=49;
  SDL_SCANCODE_NONUSHASH=50;
  SDL_SCANCODE_SEMICOLON=51;
  SDL_SCANCODE_APOSTROPHE=52;
  SDL_SCANCODE_GRAVE=53;
  SDL_SCANCODE_COMMA=54;
  SDL_SCANCODE_PERIOD=55;
  SDL_SCANCODE_SLASH=56;

  SDL_SCANCODE_CAPSLOCK=57;

  SDL_SCANCODE_F1=58;
  SDL_SCANCODE_F2=59;
  SDL_SCANCODE_F3=60;
  SDL_SCANCODE_F4=61;
  SDL_SCANCODE_F5=62;
  SDL_SCANCODE_F6=63;
  SDL_SCANCODE_F7=64;
  SDL_SCANCODE_F8=65;
  SDL_SCANCODE_F9=66;
  SDL_SCANCODE_F10=67;
  SDL_SCANCODE_F11=68;
  SDL_SCANCODE_F12=69;

  SDL_SCANCODE_PRINTSCREEN=70;
  SDL_SCANCODE_SCROLLLOCK=71;
  SDL_SCANCODE_PAUSE=72;
  SDL_SCANCODE_INSERT=73;
  SDL_SCANCODE_HOME=74;
  SDL_SCANCODE_PAGEUP=75;
  SDL_SCANCODE_DELETE=76;
  SDL_SCANCODE_END=77;
  SDL_SCANCODE_PAGEDOWN=78;
  SDL_SCANCODE_RIGHT=79;
  SDL_SCANCODE_LEFT=80;
  SDL_SCANCODE_DOWN=81;
  SDL_SCANCODE_UP=82;

  SDL_SCANCODE_NUMLOCKCLEAR=83;
  SDL_SCANCODE_KP_DIVIDE=84;
  SDL_SCANCODE_KP_MULTIPLY=85;
  SDL_SCANCODE_KP_MINUS=86;
  SDL_SCANCODE_KP_PLUS=87;
  SDL_SCANCODE_KP_ENTER=88;
  SDL_SCANCODE_KP_1=89;
  SDL_SCANCODE_KP_2=90;
  SDL_SCANCODE_KP_3=91;
  SDL_SCANCODE_KP_4=92;
  SDL_SCANCODE_KP_5=93;
  SDL_SCANCODE_KP_6=94;
  SDL_SCANCODE_KP_7=95;
  SDL_SCANCODE_KP_8=96;
  SDL_SCANCODE_KP_9=97;
  SDL_SCANCODE_KP_0=98;
  SDL_SCANCODE_KP_PERIOD=99;

  SDL_SCANCODE_NONUSBACKSLASH=100;
  SDL_SCANCODE_APPLICATION=101;
  SDL_SCANCODE_POWER=102;
  SDL_SCANCODE_KP_EQUALS=103;

  SDL_SCANCODE_F13=104;
  SDL_SCANCODE_F14=105;
  SDL_SCANCODE_F15=106;
  SDL_SCANCODE_F16=107;
  SDL_SCANCODE_F17=108;
  SDL_SCANCODE_F18=109;
  SDL_SCANCODE_F19=110;
  SDL_SCANCODE_F20=111;
  SDL_SCANCODE_F21=112;
  SDL_SCANCODE_F22=113;
  SDL_SCANCODE_F23=114;
  SDL_SCANCODE_F24=115;
  SDL_SCANCODE_EXECUTE=116;
  SDL_SCANCODE_HELP=117;
  SDL_SCANCODE_MENU=118;
  SDL_SCANCODE_SELECT=119;
  SDL_SCANCODE_STOP=120;
  SDL_SCANCODE_AGAIN=121;
  SDL_SCANCODE_UNDO=122;
  SDL_SCANCODE_CUT=123;
  SDL_SCANCODE_COPY=124;
  SDL_SCANCODE_PASTE=125;
  SDL_SCANCODE_FIND=126;
  SDL_SCANCODE_MUTE=127;
  SDL_SCANCODE_VOLUMEUP=128;
  SDL_SCANCODE_VOLUMEDOWN=129;
  SDL_SCANCODE_KP_COMMA=133;
  SDL_SCANCODE_KP_EQUALSAS400=134;

  SDL_SCANCODE_INTERNATIONAL1=135;
  SDL_SCANCODE_INTERNATIONAL2=136;
  SDL_SCANCODE_INTERNATIONAL3=137;
  SDL_SCANCODE_INTERNATIONAL4=138;
  SDL_SCANCODE_INTERNATIONAL5=139;
  SDL_SCANCODE_INTERNATIONAL6=140;
  SDL_SCANCODE_INTERNATIONAL7=141;
  SDL_SCANCODE_INTERNATIONAL8=142;
  SDL_SCANCODE_INTERNATIONAL9=143;
  SDL_SCANCODE_LANG1=144;
  SDL_SCANCODE_LANG2=145;
  SDL_SCANCODE_LANG3=146;
  SDL_SCANCODE_LANG4=147;
  SDL_SCANCODE_LANG5=148;
  SDL_SCANCODE_LANG6=149;
  SDL_SCANCODE_LANG7=150;
  SDL_SCANCODE_LANG8=151;
  SDL_SCANCODE_LANG9=152;

  SDL_SCANCODE_ALTERASE=153;
  SDL_SCANCODE_SYSREQ=154;
  SDL_SCANCODE_CANCEL=155;
  SDL_SCANCODE_CLEAR=156;
  SDL_SCANCODE_PRIOR=157;
  SDL_SCANCODE_RETURN2=158;
  SDL_SCANCODE_SEPARATOR=159;
  SDL_SCANCODE_OUT=160;
  SDL_SCANCODE_OPER=161;
  SDL_SCANCODE_CLEARAGAIN=162;
  SDL_SCANCODE_CRSEL=163;
  SDL_SCANCODE_EXSEL=164;

  SDL_SCANCODE_KP_00=176;
  SDL_SCANCODE_KP_000=177;
  SDL_SCANCODE_THOUSANDSSEPARATOR=178;
  SDL_SCANCODE_DECIMALSEPARATOR=179;
  SDL_SCANCODE_CURRENCYUNIT=180;
  SDL_SCANCODE_CURRENCYSUBUNIT=181;
  SDL_SCANCODE_KP_LEFTPAREN=182;
  SDL_SCANCODE_KP_RIGHTPAREN=183;
  SDL_SCANCODE_KP_LEFTBRACE=184;
  SDL_SCANCODE_KP_RIGHTBRACE=185;
  SDL_SCANCODE_KP_TAB=186;
  SDL_SCANCODE_KP_BACKSPACE=187;
  SDL_SCANCODE_KP_A=188;
  SDL_SCANCODE_KP_B=189;
  SDL_SCANCODE_KP_C=190;
  SDL_SCANCODE_KP_D=191;
  SDL_SCANCODE_KP_E=192;
  SDL_SCANCODE_KP_F=193;
  SDL_SCANCODE_KP_XOR=194;
  SDL_SCANCODE_KP_POWER=195;
  SDL_SCANCODE_KP_PERCENT=196;
  SDL_SCANCODE_KP_LESS=197;
  SDL_SCANCODE_KP_GREATER=198;
  SDL_SCANCODE_KP_AMPERSAND=199;
  SDL_SCANCODE_KP_DBLAMPERSAND=200;
  SDL_SCANCODE_KP_VERTICALBAR=201;
  SDL_SCANCODE_KP_DBLVERTICALBAR=202;
  SDL_SCANCODE_KP_COLON=203;
  SDL_SCANCODE_KP_HASH=204;
  SDL_SCANCODE_KP_SPACE=205;
  SDL_SCANCODE_KP_AT=206;
  SDL_SCANCODE_KP_EXCLAM=207;
  SDL_SCANCODE_KP_MEMSTORE=208;
  SDL_SCANCODE_KP_MEMRECALL=209;
  SDL_SCANCODE_KP_MEMCLEAR=210;
  SDL_SCANCODE_KP_MEMADD=211;
  SDL_SCANCODE_KP_MEMSUBTRACT=212;
  SDL_SCANCODE_KP_MEMMULTIPLY=213;
  SDL_SCANCODE_KP_MEMDIVIDE=214;
  SDL_SCANCODE_KP_PLUSMINUS=215;
  SDL_SCANCODE_KP_CLEAR=216;
  SDL_SCANCODE_KP_CLEARENTRY=217;
  SDL_SCANCODE_KP_BINARY=218;
  SDL_SCANCODE_KP_OCTAL=219;
  SDL_SCANCODE_KP_DECIMAL=220;
  SDL_SCANCODE_KP_HEXADECIMAL=221;

  SDL_SCANCODE_LCTRL=224;
  SDL_SCANCODE_LSHIFT=225;
  SDL_SCANCODE_LALT=226;
  SDL_SCANCODE_LGUI=227;
  SDL_SCANCODE_RCTRL=228;
  SDL_SCANCODE_RSHIFT=229;
  SDL_SCANCODE_RALT=230;
  SDL_SCANCODE_RGUI=231;

  SDL_SCANCODE_MODE=257;

  SDL_SCANCODE_AUDIONEXT=258;
  SDL_SCANCODE_AUDIOPREV=259;
  SDL_SCANCODE_AUDIOSTOP=260;
  SDL_SCANCODE_AUDIOPLAY=261;
  SDL_SCANCODE_AUDIOMUTE=262;
  SDL_SCANCODE_MEDIASELECT=263;
  SDL_SCANCODE_WWW=264;
  SDL_SCANCODE_MAIL=265;
  SDL_SCANCODE_CALCULATOR=266;
  SDL_SCANCODE_COMPUTER=267;
  SDL_SCANCODE_AC_SEARCH=268;
  SDL_SCANCODE_AC_HOME=269;
  SDL_SCANCODE_AC_BACK=270;
  SDL_SCANCODE_AC_FORWARD=271;
  SDL_SCANCODE_AC_STOP=272;
  SDL_SCANCODE_AC_REFRESH=273;
  SDL_SCANCODE_AC_BOOKMARKS=274;

  SDL_SCANCODE_BRIGHTNESSDOWN=275;
  SDL_SCANCODE_BRIGHTNESSUP=276;
  SDL_SCANCODE_DISPLAYSWITCH=277;
  SDL_SCANCODE_KBDILLUMTOGGLE=278;
  SDL_SCANCODE_KBDILLUMDOWN=279;
  SDL_SCANCODE_KBDILLUMUP=280;
  SDL_SCANCODE_EJECT=281;
  SDL_SCANCODE_SLEEP=282;

  SDL_SCANCODE_APP1=283;
  SDL_SCANCODE_APP2=284;

  SDL_NUM_SCANCODES=512;

//=====SDL_KEYCODE=====

//scancode to keycode
{$DEFINE S2K:=or 1 shl 30}

const
  SDLK_UNKNOWN=0;

  SDLK_RETURN=$D;
  SDLK_ESCAPE=$1B;
  SDLK_BACKSPACE=$8;
  SDLK_TAB=$9;
  SDLK_SPACE=Ord(' ');
  SDLK_EXCLAIM=Ord('!');
  SDLK_QUOTEDBL=Ord('"');
  SDLK_HASH=Ord('#');
  SDLK_DOLLAR=Ord('$');
  SDLK_PERCENT=Ord('%');
  SDLK_AMPERSAND=Ord('&');
  SDLK_QUOTE=Ord('''');
  SDLK_LEFTPAREN=Ord('(');
  SDLK_RIGHTPAREN=Ord(')');
  SDLK_ASTERISK=Ord('*');
  SDLK_PLUS=Ord('+');
  SDLK_COMMA=Ord(',');
  SDLK_MINUS=Ord('-');
  SDLK_PERIOD=Ord('.');
  SDLK_SLASH=Ord('/');
  SDLK_0=Ord('0');
  SDLK_1=Ord('1');
  SDLK_2=Ord('2');
  SDLK_3=Ord('3');
  SDLK_4=Ord('4');
  SDLK_5=Ord('5');
  SDLK_6=Ord('6');
  SDLK_7=Ord('7');
  SDLK_8=Ord('8');
  SDLK_9=Ord('9');
  SDLK_COLON=Ord(':');
  SDLK_SEMICOLON=Ord(';');
  SDLK_LESS=Ord('<');
  SDLK_EQUALS=Ord('=');
  SDLK_GREATER=Ord('>');
  SDLK_QUESTION=Ord('?');
  SDLK_AT=Ord('@');

  SDLK_LEFTBRACKET=Ord('[');
  SDLK_BACKSLASH=Ord('\');
  SDLK_RIGHTBRACKET=Ord(']');
  SDLK_CARET=Ord('^');
  SDLK_UNDERSCORE=Ord('_');
  SDLK_BACKQUOTE=Ord('`');
  SDLK_a=Ord('a');
  SDLK_b=Ord('b');
  SDLK_c=Ord('c');
  SDLK_d=Ord('d');
  SDLK_e=Ord('e');
  SDLK_f=Ord('f');
  SDLK_g=Ord('g');
  SDLK_h=Ord('h');
  SDLK_i=Ord('i');
  SDLK_j=Ord('j');
  SDLK_k=Ord('k');
  SDLK_l=Ord('l');
  SDLK_m=Ord('m');
  SDLK_n=Ord('n');
  SDLK_o=Ord('o');
  SDLK_p=Ord('p');
  SDLK_q=Ord('q');
  SDLK_r=Ord('r');
  SDLK_s=Ord('s');
  SDLK_t=Ord('t');
  SDLK_u=Ord('u');
  SDLK_v=Ord('v');
  SDLK_w=Ord('w');
  SDLK_x=Ord('x');
  SDLK_y=Ord('y');
  SDLK_z=Ord('z');

  SDLK_CAPSLOCK=SDL_SCANCODE_CAPSLOCK S2K;

  SDLK_F1=SDL_SCANCODE_F1 S2K;
  SDLK_F2=SDL_SCANCODE_F2 S2K;
  SDLK_F3=SDL_SCANCODE_F3 S2K;
  SDLK_F4=SDL_SCANCODE_F4 S2K;
  SDLK_F5=SDL_SCANCODE_F5 S2K;
  SDLK_F6=SDL_SCANCODE_F6 S2K;
  SDLK_F7=SDL_SCANCODE_F7 S2K;
  SDLK_F8=SDL_SCANCODE_F8 S2K;
  SDLK_F9=SDL_SCANCODE_F9 S2K;
  SDLK_F10=SDL_SCANCODE_F10 S2K;
  SDLK_F11=SDL_SCANCODE_F11 S2K;
  SDLK_F12=SDL_SCANCODE_F12 S2K;

  SDLK_PRINTSCREEN=SDL_SCANCODE_PRINTSCREEN S2K;
  SDLK_SCROLLLOCK=SDL_SCANCODE_SCROLLLOCK S2K;
  SDLK_PAUSE=SDL_SCANCODE_PAUSE S2K;
  SDLK_INSERT=SDL_SCANCODE_INSERT S2K;
  SDLK_HOME=SDL_SCANCODE_HOME S2K;
  SDLK_PAGEUP=SDL_SCANCODE_PAGEUP S2K;
  SDLK_DELETE=$7F;
  SDLK_END=SDL_SCANCODE_END S2K;
  SDLK_PAGEDOWN=SDL_SCANCODE_PAGEDOWN S2K;
  SDLK_RIGHT=SDL_SCANCODE_RIGHT S2K;
  SDLK_LEFT=SDL_SCANCODE_LEFT S2K;
  SDLK_DOWN=SDL_SCANCODE_DOWN S2K;
  SDLK_UP=SDL_SCANCODE_UP S2K;

  SDLK_NUMLOCKCLEAR=SDL_SCANCODE_NUMLOCKCLEAR S2K;
  SDLK_KP_DIVIDE=SDL_SCANCODE_KP_DIVIDE S2K;
  SDLK_KP_MULTIPLY=SDL_SCANCODE_KP_MULTIPLY S2K;
  SDLK_KP_MINUS=SDL_SCANCODE_KP_MINUS S2K;
  SDLK_KP_PLUS=SDL_SCANCODE_KP_PLUS S2K;
  SDLK_KP_ENTER=SDL_SCANCODE_KP_ENTER S2K;
  SDLK_KP_1=SDL_SCANCODE_KP_1 S2K;
  SDLK_KP_2=SDL_SCANCODE_KP_2 S2K;
  SDLK_KP_3=SDL_SCANCODE_KP_3 S2K;
  SDLK_KP_4=SDL_SCANCODE_KP_4 S2K;
  SDLK_KP_5=SDL_SCANCODE_KP_5 S2K;
  SDLK_KP_6=SDL_SCANCODE_KP_6 S2K;
  SDLK_KP_7=SDL_SCANCODE_KP_7 S2K;
  SDLK_KP_8=SDL_SCANCODE_KP_8 S2K;
  SDLK_KP_9=SDL_SCANCODE_KP_9 S2K;
  SDLK_KP_0=SDL_SCANCODE_KP_0 S2K;
  SDLK_KP_PERIOD=SDL_SCANCODE_KP_PERIOD S2K;

  SDLK_APPLICATION=SDL_SCANCODE_APPLICATION S2K;
  SDLK_POWER=SDL_SCANCODE_POWER S2K;
  SDLK_KP_EQUALS=SDL_SCANCODE_KP_EQUALS S2K;
  SDLK_F13=SDL_SCANCODE_F13 S2K;
  SDLK_F14=SDL_SCANCODE_F14 S2K;
  SDLK_F15=SDL_SCANCODE_F15 S2K;
  SDLK_F16=SDL_SCANCODE_F16 S2K;
  SDLK_F17=SDL_SCANCODE_F17 S2K;
  SDLK_F18=SDL_SCANCODE_F18 S2K;
  SDLK_F19=SDL_SCANCODE_F19 S2K;
  SDLK_F20=SDL_SCANCODE_F20 S2K;
  SDLK_F21=SDL_SCANCODE_F21 S2K;
  SDLK_F22=SDL_SCANCODE_F22 S2K;
  SDLK_F23=SDL_SCANCODE_F23 S2K;
  SDLK_F24=SDL_SCANCODE_F24 S2K;
  SDLK_EXECUTE=SDL_SCANCODE_EXECUTE S2K;
  SDLK_HELP=SDL_SCANCODE_HELP S2K;
  SDLK_MENU=SDL_SCANCODE_MENU S2K;
  SDLK_SELECT=SDL_SCANCODE_SELECT S2K;
  SDLK_STOP=SDL_SCANCODE_STOP S2K;
  SDLK_AGAIN=SDL_SCANCODE_AGAIN S2K;
  SDLK_UNDO=SDL_SCANCODE_UNDO S2K;
  SDLK_CUT=SDL_SCANCODE_CUT S2K;
  SDLK_COPY=SDL_SCANCODE_COPY S2K;
  SDLK_PASTE=SDL_SCANCODE_PASTE S2K;
  SDLK_FIND=SDL_SCANCODE_FIND S2K;
  SDLK_MUTE=SDL_SCANCODE_MUTE S2K;
  SDLK_VOLUMEUP=SDL_SCANCODE_VOLUMEUP S2K;
  SDLK_VOLUMEDOWN=SDL_SCANCODE_VOLUMEDOWN S2K;
  SDLK_KP_COMMA=SDL_SCANCODE_KP_COMMA S2K;
  SDLK_KP_EQUALSAS400=SDL_SCANCODE_KP_EQUALSAS400 S2K;

  SDLK_ALTERASE=SDL_SCANCODE_ALTERASE S2K;
  SDLK_SYSREQ=SDL_SCANCODE_SYSREQ S2K;
  SDLK_CANCEL=SDL_SCANCODE_CANCEL S2K;
  SDLK_CLEAR=SDL_SCANCODE_CLEAR S2K;
  SDLK_PRIOR=SDL_SCANCODE_PRIOR S2K;
  SDLK_RETURN2=SDL_SCANCODE_RETURN2 S2K;
  SDLK_SEPARATOR=SDL_SCANCODE_SEPARATOR S2K;
  SDLK_OUT=SDL_SCANCODE_OUT S2K;
  SDLK_OPER=SDL_SCANCODE_OPER S2K;
  SDLK_CLEARAGAIN=SDL_SCANCODE_CLEARAGAIN S2K;
  SDLK_CRSEL=SDL_SCANCODE_CRSEL S2K;
  SDLK_EXSEL=SDL_SCANCODE_EXSEL S2K;

  SDLK_KP_00=SDL_SCANCODE_KP_00 S2K;
  SDLK_KP_000=SDL_SCANCODE_KP_000 S2K;
  SDLK_THOUSANDSSEPARATOR=SDL_SCANCODE_THOUSANDSSEPARATOR S2K;
  SDLK_DECIMALSEPARATOR=SDL_SCANCODE_DECIMALSEPARATOR S2K;
  SDLK_CURRENCYUNIT=SDL_SCANCODE_CURRENCYUNIT S2K;
  SDLK_CURRENCYSUBUNIT=SDL_SCANCODE_CURRENCYSUBUNIT S2K;
  SDLK_KP_LEFTPAREN=SDL_SCANCODE_KP_LEFTPAREN S2K;
  SDLK_KP_RIGHTPAREN=SDL_SCANCODE_KP_RIGHTPAREN S2K;
  SDLK_KP_LEFTBRACE=SDL_SCANCODE_KP_LEFTBRACE S2K;
  SDLK_KP_RIGHTBRACE=SDL_SCANCODE_KP_RIGHTBRACE S2K;
  SDLK_KP_TAB=SDL_SCANCODE_KP_TAB S2K;
  SDLK_KP_BACKSPACE=SDL_SCANCODE_KP_BACKSPACE S2K;
  SDLK_KP_A=SDL_SCANCODE_KP_A S2K;
  SDLK_KP_B=SDL_SCANCODE_KP_B S2K;
  SDLK_KP_C=SDL_SCANCODE_KP_C S2K;
  SDLK_KP_D=SDL_SCANCODE_KP_D S2K;
  SDLK_KP_E=SDL_SCANCODE_KP_E S2K;
  SDLK_KP_F=SDL_SCANCODE_KP_F S2K;
  SDLK_KP_XOR=SDL_SCANCODE_KP_XOR S2K;
  SDLK_KP_POWER=SDL_SCANCODE_KP_POWER S2K;
  SDLK_KP_PERCENT=SDL_SCANCODE_KP_PERCENT S2K;
  SDLK_KP_LESS=SDL_SCANCODE_KP_LESS S2K;
  SDLK_KP_GREATER=SDL_SCANCODE_KP_GREATER S2K;
  SDLK_KP_AMPERSAND=SDL_SCANCODE_KP_AMPERSAND S2K;
  SDLK_KP_DBLAMPERSAND=SDL_SCANCODE_KP_DBLAMPERSAND S2K;
  SDLK_KP_VERTICALBAR=SDL_SCANCODE_KP_VERTICALBAR S2K;
  SDLK_KP_DBLVERTICALBAR=SDL_SCANCODE_KP_DBLVERTICALBAR S2K;
  SDLK_KP_COLON=SDL_SCANCODE_KP_COLON S2K;
  SDLK_KP_HASH=SDL_SCANCODE_KP_HASH S2K;
  SDLK_KP_SPACE=SDL_SCANCODE_KP_SPACE S2K;
  SDLK_KP_AT=SDL_SCANCODE_KP_AT S2K;
  SDLK_KP_EXCLAM=SDL_SCANCODE_KP_EXCLAM S2K;
  SDLK_KP_MEMSTORE=SDL_SCANCODE_KP_MEMSTORE S2K;
  SDLK_KP_MEMRECALL=SDL_SCANCODE_KP_MEMRECALL S2K;
  SDLK_KP_MEMCLEAR=SDL_SCANCODE_KP_MEMCLEAR S2K;
  SDLK_KP_MEMADD=SDL_SCANCODE_KP_MEMADD S2K;
  SDLK_KP_MEMSUBTRACT=SDL_SCANCODE_KP_MEMSUBTRACT S2K;
  SDLK_KP_MEMMULTIPLY=SDL_SCANCODE_KP_MEMMULTIPLY S2K;
  SDLK_KP_MEMDIVIDE=SDL_SCANCODE_KP_MEMDIVIDE S2K;
  SDLK_KP_PLUSMINUS=SDL_SCANCODE_KP_PLUSMINUS S2K;
  SDLK_KP_CLEAR=SDL_SCANCODE_KP_CLEAR S2K;
  SDLK_KP_CLEARENTRY=SDL_SCANCODE_KP_CLEARENTRY S2K;
  SDLK_KP_BINARY=SDL_SCANCODE_KP_BINARY S2K;
  SDLK_KP_OCTAL=SDL_SCANCODE_KP_OCTAL S2K;
  SDLK_KP_DECIMAL=SDL_SCANCODE_KP_DECIMAL S2K;
  SDLK_KP_HEXADECIMAL=SDL_SCANCODE_KP_HEXADECIMAL S2K;

  SDLK_LCTRL=SDL_SCANCODE_LCTRL S2K;
  SDLK_LSHIFT=SDL_SCANCODE_LSHIFT S2K;
  SDLK_LALT=SDL_SCANCODE_LALT S2K;
  SDLK_LGUI=SDL_SCANCODE_LGUI S2K;
  SDLK_RCTRL=SDL_SCANCODE_RCTRL S2K;
  SDLK_RSHIFT=SDL_SCANCODE_RSHIFT S2K;
  SDLK_RALT=SDL_SCANCODE_RALT S2K;
  SDLK_RGUI=SDL_SCANCODE_RGUI S2K;

  SDLK_MODE=SDL_SCANCODE_MODE S2K;

  SDLK_AUDIONEXT=SDL_SCANCODE_AUDIONEXT S2K;
  SDLK_AUDIOPREV=SDL_SCANCODE_AUDIOPREV S2K;
  SDLK_AUDIOSTOP=SDL_SCANCODE_AUDIOSTOP S2K;
  SDLK_AUDIOPLAY=SDL_SCANCODE_AUDIOPLAY S2K;
  SDLK_AUDIOMUTE=SDL_SCANCODE_AUDIOMUTE S2K;
  SDLK_MEDIASELECT=SDL_SCANCODE_MEDIASELECT S2K;
  SDLK_WWW=SDL_SCANCODE_WWW S2K;
  SDLK_MAIL=SDL_SCANCODE_MAIL S2K;
  SDLK_CALCULATOR=SDL_SCANCODE_CALCULATOR S2K;
  SDLK_COMPUTER=SDL_SCANCODE_COMPUTER S2K;
  SDLK_AC_SEARCH=SDL_SCANCODE_AC_SEARCH S2K;
  SDLK_AC_HOME=SDL_SCANCODE_AC_HOME S2K;
  SDLK_AC_BACK=SDL_SCANCODE_AC_BACK S2K;
  SDLK_AC_FORWARD=SDL_SCANCODE_AC_FORWARD S2K;
  SDLK_AC_STOP=SDL_SCANCODE_AC_STOP S2K;
  SDLK_AC_REFRESH=SDL_SCANCODE_AC_REFRESH S2K;
  SDLK_AC_BOOKMARKS=SDL_SCANCODE_AC_BOOKMARKS S2K;

  SDLK_BRIGHTNESSDOWN=SDL_SCANCODE_BRIGHTNESSDOWN S2K;
  SDLK_BRIGHTNESSUP=SDL_SCANCODE_BRIGHTNESSUP S2K;
  SDLK_DISPLAYSWITCH=SDL_SCANCODE_DISPLAYSWITCH S2K;
  SDLK_KBDILLUMTOGGLE=SDL_SCANCODE_KBDILLUMTOGGLE S2K;
  SDLK_KBDILLUMDOWN=SDL_SCANCODE_KBDILLUMDOWN S2K;
  SDLK_KBDILLUMUP=SDL_SCANCODE_KBDILLUMUP S2K;
  SDLK_EJECT=SDL_SCANCODE_EJECT S2K;
  SDLK_SLEEP=SDL_SCANCODE_SLEEP S2K;

  KMOD_NONE=$0000;
  KMOD_LSHIFT=$0001;
  KMOD_RSHIFT=$0002;
  KMOD_LCTRL=$0040;
  KMOD_RCTRL=$0080;
  KMOD_LALT=$0100;
  KMOD_RALT=$0200;
  KMOD_LGUI=$0400;
  KMOD_RGUI=$0800;
  KMOD_NUM=$1000;
  KMOD_CAPS=$2000;
  KMOD_MODE=$4000;
  KMOD_RESERVED=$8000;

  KMOD_CTRL=KMOD_LCTRL or KMOD_RCTRL;
  KMOD_SHIFT=KMOD_LSHIFT or KMOD_RSHIFT;
  KMOD_ALT=KMOD_LALT or KMOD_RALT;
  KMOD_GUI=KMOD_LGUI or KMOD_RGUI;

type
  PSDL_KeyCode=^TSDL_KeyCode;
  TSDL_KeyCode=longint;

  PSDL_KeyMod=^TSDL_KeyMod;
  TSDL_KeyMod=word;

//=====SDL_KEYBOARD=====

type
  PSDL_Keysym=^TSDL_Keysym;
  TSDL_Keysym=record
    scancode: TSDL_ScanCode;
    sym: TSDL_KeyCode;
    mod_: UInt16;
    unused: Uint32;
  end;

function SDL_GetKeyboardFocus: PSDL_Window; lSDL;
function SDL_GetKeyboardState(numkeys: plongint): PUint8; lSDL;
function SDL_GetModState: TSDL_KeyMod; lSDL;
procedure SDL_SetModState(modstate: TSDL_KeyMod); lSDL;
function SDL_GetKeyFromScancode(scancode: TSDL_ScanCode): TSDL_KeyCode; lSDL;
function SDL_GetScancodeFromKey(key: TSDL_KeyCode): TSDL_ScanCode; lSDL;
function SDL_GetScancodeName(scancode: TSDL_ScanCode): pchar; lSDL;
function SDL_GetScancodeFromName(const name: pchar): TSDL_ScanCode; lSDL;
function SDL_GetKeyName(key: TSDL_KeyCode): pchar; lSDL;
function SDL_GetKeyFromName(const name: pchar): TSDL_ScanCode; lSDL;
procedure SDL_StartTextInput; lSDL;
function SDL_IsTextInputActive: SDL_bool; lSDL;
procedure SDL_StopTextInput; lSDL;
procedure SDL_SetTextInputRect(rect: PSDL_Rect); lSDL;
function SDL_HasScreenKeyboardSupport: SDL_bool; lSDL;
function SDL_IsScreenKeyboardShown(window: PSDL_Window): SDL_bool; lSDL;

//=====SDL_MOUSE=====

type
  PSDL_Cursor=pointer;

const
  SDL_SYSTEM_CURSOR_ARROW=0;
  SDL_SYSTEM_CURSOR_IBEAM=1;
  SDL_SYSTEM_CURSOR_WAIT=2;
  SDL_SYSTEM_CURSOR_CROSSHAIR=3;
  SDL_SYSTEM_CURSOR_WAITARROW=4;
  SDL_SYSTEM_CURSOR_SIZENWSE=5;
  SDL_SYSTEM_CURSOR_SIZENESW=6;
  SDL_SYSTEM_CURSOR_SIZEWE=7;
  SDL_SYSTEM_CURSOR_SIZENS=8;
  SDL_SYSTEM_CURSOR_SIZEALL=9;
  SDL_SYSTEM_CURSOR_NO=10;
  SDL_SYSTEM_CURSOR_HAND=11;
  SDL_NUM_SYSTEM_CURSORS=12;

  SDL_BUTTON_LEFT  =1;
  SDL_BUTTON_MIDDLE=2;
  SDL_BUTTON_RIGHT =3;
  SDL_BUTTON_X1    =4;
  SDL_BUTTON_X2    =5;
  SDL_BUTTON_LMASK =1 shl ((SDL_BUTTON_LEFT)-1);
  SDL_BUTTON_MMASK =1 shl ((SDL_BUTTON_MIDDLE)-1);
  SDL_BUTTON_RMASK =1 shl ((SDL_BUTTON_RIGHT)-1);
  SDL_BUTTON_X1MASK=1 shl ((SDL_BUTTON_X1)-1);
  SDL_BUTTON_X2MASK=1 shl ((SDL_BUTTON_X2)-1);

function SDL_GetMouseFocus: PSDL_Window; lSDL;
function SDL_GetMouseState(x, y: plongint): Uint32; lSDL;
function SDL_GetRelativeMouseState(x, y: plongint): Uint32; lSDL;
procedure SDL_WarpMouseInWindow(window: PSDL_Window; x, y: longint); lSDL;
function SDL_SetRelativeMouseMode(enabled: SDL_bool): longint; lSDL;
function SDL_GetRelativeMouseMode: SDL_bool; lSDL;

function SDL_CreateCursor(const data , mask: PUint8;
                          w, h, hot_x, hot_y: longint): PSDL_Cursor; lSDL;
function SDL_CreateColorCursor(surface: PSDL_Surface;
                               hot_x, hot_y: longint): PSDL_Cursor; lSDL;
function SDL_CreateSystemCursor(id: longword): PSDL_Cursor; lSDL;
procedure SDL_SetCursor(cursor: PSDL_Cursor); lSDL;
function SDL_GetCursor: PSDL_Cursor; lSDL;
procedure SDL_FreeCursor(cursor: PSDL_Cursor); lSDL;
function SDL_ShowCursor(toggle: longint): longint; lSDL;

//=====SDL_JOYSTICK=====

type
  PSDL_Joystick=pointer;

  TSDL_JoystickGUID=record
    data: array[0..15] of UInt8;
  end;

  TSDL_JoystickID=Sint32;

const
  SDL_HAT_CENTERED =$00;
  SDL_HAT_UP       =$01;
  SDL_HAT_RIGHT    =$02;
  SDL_HAT_DOWN     =$04;
  SDL_HAT_LEFT     =$08;
  SDL_HAT_RIGHTUP  =SDL_HAT_RIGHT or SDL_HAT_UP;
  SDL_HAT_RIGHTDOWN=SDL_HAT_RIGHT or SDL_HAT_DOWN;
  SDL_HAT_LEFTUP   =SDL_HAT_LEFT or SDL_HAT_UP;
  SDL_HAT_LEFTDOWN =SDL_HAT_LEFT or SDL_HAT_DOWN;

function SDL_NumJoysticks: longint; lSDL;
function SDL_JoystickNameForIndex(device_index: longint): pchar; lSDL;
function SDL_JoystickOpen(device_index: longint): PSDL_Joystick; lSDL;
function SDL_JoystickName(joystick: PSDL_Joystick): pchar; lSDL;
function SDL_JoystickGetDeviceGUID(device_index: longint): TSDL_JoystickGUID; lSDL;
function SDL_JoystickGetGUID(joystick: PSDL_Joystick): TSDL_JoystickGUID; lSDL;
procedure SDL_JoystickGetGUIDString(guid: TSDL_JoystickGUID; pszGUID: pchar;
                                    cbGUID: longint); lSDL;
function SDL_JoystickGetGUIDFromString(pszGUID: pchar): TSDL_JoystickGUID; lSDL;
function SDL_JoystickGetAttached(joystick: PSDL_Joystick): SDL_bool; lSDL;
function SDL_JoystickInstanceID(joystick: PSDL_Joystick): TSDL_JoystickID; lSDL;
function SDL_JoystickNumAxes(joystick: PSDL_Joystick): longint; lSDL;
function SDL_JoystickNumBalls(joystick: PSDL_Joystick): longint; lSDL;
function SDL_JoystickNumHats(joystick: PSDL_Joystick): longint; lSDL;
function SDL_JoystickNumButtons(joystick: PSDL_Joystick): longint; lSDL;
procedure SDL_JoystickUpdate; lSDL;
function SDL_JoystickEventState(state: longint): longint; lSDL;
function SDL_JoystickGetAxis(joystick: PSDL_Joystick;
                             axis: longint): Sint16; lSDL;
function SDL_JoystickGetHat(joystick: PSDL_Joystick; hat: longint): Uint8; lSDL;
function SDL_JoystickGetBall(joystick: PSDL_Joystick; ball: longint;
                             dx, dy: plongint): longint; lSDL;
function SDL_JoystickGetButton(joystick: PSDL_Joystick;
                               button: longint): Uint8; lSDL;
procedure SDL_JoystickClose(joystick: PSDL_Joystick); lSDL;

//=====SDL_GAMECONTROLLER=====

type
  SDL_GameControllerBindType=(
    SDL_CONTROLLER_BINDTYPE_NONE=0,
    SDL_CONTROLLER_BINDTYPE_BUTTON,
    SDL_CONTROLLER_BINDTYPE_AXIS,
    SDL_CONTROLLER_BINDTYPE_HAT);

  SDL_GameControllerAxis=(
    SDL_CONTROLLER_AXIS_INVALID=-1,
    SDL_CONTROLLER_AXIS_LEFTX,
    SDL_CONTROLLER_AXIS_LEFTY,
    SDL_CONTROLLER_AXIS_RIGHTX,
    SDL_CONTROLLER_AXIS_RIGHTY,
    SDL_CONTROLLER_AXIS_TRIGGERLEFT,
    SDL_CONTROLLER_AXIS_TRIGGERRIGHT,
    SDL_CONTROLLER_AXIS_MAX);

  SDL_GameControllerButton=(
    SDL_CONTROLLER_BUTTON_INVALID=-1,
    SDL_CONTROLLER_BUTTON_A,
    SDL_CONTROLLER_BUTTON_B,
    SDL_CONTROLLER_BUTTON_X,
    SDL_CONTROLLER_BUTTON_Y,
    SDL_CONTROLLER_BUTTON_BACK,
    SDL_CONTROLLER_BUTTON_GUIDE,
    SDL_CONTROLLER_BUTTON_START,
    SDL_CONTROLLER_BUTTON_LEFTSTICK,
    SDL_CONTROLLER_BUTTON_RIGHTSTICK,
    SDL_CONTROLLER_BUTTON_LEFTSHOULDER,
    SDL_CONTROLLER_BUTTON_RIGHTSHOULDER,
    SDL_CONTROLLER_BUTTON_DPAD_UP,
    SDL_CONTROLLER_BUTTON_DPAD_DOWN,
    SDL_CONTROLLER_BUTTON_DPAD_LEFT,
    SDL_CONTROLLER_BUTTON_DPAD_RIGHT,
    SDL_CONTROLLER_BUTTON_MAX);

  PSDL_GameController=pointer;

  PSDL_GameControllerButtonBind=^TSDL_GameControllerButtonBind;
  TSDL_GameControllerButtonBind=record
    bindType: SDL_GameControllerBindType;
    value: record
      case longint of
        1: (button: longint);
        2: (axis: longint);
        3: (hat: record
              hat,
              hat_mask: longint;
            end);
    end;
  end;

function SDL_GameControllerAddMappingsFromRW(rw: PSDL_RWops;
                                             freerw: longint): longint; lSDL;
function SDL_GameControllerAddMappingsFromFile(file_: pchar): longint; inline;

function SDL_GameControllerAddMapping(mapping: pchar): longint; lSDL;
function SDL_GameControllerMappingForGUID(guid: TSDL_JoystickGUID): pchar; lSDL;
function SDL_GameControllerMapping(gamecontroller: PSDL_GameController): pchar; lSDL;
function SDL_IsGameController(joystick_index: longint): SDL_bool; lSDL;
function SDL_GameControllerNameForIndex(joystick_index: longint): pchar; lSDL;
function SDL_GameControllerOpen(joystick_index: longint): PSDL_GameController; lSDL;
function SDL_GameControllerName(gamecontroller: PSDL_GameController): pchar; lSDL;
function SDL_GameControllerGetAttached(gamecontroller: PSDL_GameController): SDL_bool; lSDL;
function SDL_GameControllerGetJoystick(gamecontroller: PSDL_GameController): PSDL_Joystick; lSDL;
function SDL_GameControllerEventState(state: longint): longint; lSDL;
procedure SDL_GameControllerUpdate; lSDL;

function SDL_GameControllerGetAxisFromString(pchString: pchar):SDL_GameControllerAxis; lSDL;
function SDL_GameControllerGetStringForAxis(axis: SDL_GameControllerAxis): pchar; lSDL;
function SDL_GameControllerGetBindForAxis(gamecontroller: PSDL_GameController;
             axis: SDL_GameControllerAxis): TSDL_GameControllerButtonBind; lSDL;
function SDL_GameControllerGetAxis(gamecontroller: PSDL_GameController;
                                    axis: SDL_GameControllerAxis): Sint16; lSDL;
function SDL_GameControllerGetButtonFromString(pchString: pchar): Uint32; lSDL;
function SDL_GameControllerGetStringForButton(button: SDL_GameControllerButton): pchar; lSDL;
function SDL_GameControllerGetBindForButton(gamecontroller: PSDL_GameController;
          button: SDL_GameControllerButton): TSDL_GameControllerButtonBind; lSDL;
function SDL_GameControllerGetButton(gamecontroller: PSDL_GameController;
                                     button: SDL_GameControllerButton): Uint8; lSDL;
procedure SDL_GameControllerClose(gamecontroller: PSDL_GameController); lSDL;

//=====SDL_HAPTIC=====

const
  SDL_HAPTIC_CONSTANT=1 shl 0;
  SDL_HAPTIC_SINE=1 shl 1;
  SDL_HAPTIC_SQUARE=1 shl 2;
  SDL_HAPTIC_TRIANGLE=1 shl 3;
  SDL_HAPTIC_SAWTOOTHUP=1 shl 4;
  SDL_HAPTIC_SAWTOOTHDOWN=1 shl 5;
  SDL_HAPTIC_RAMP=1 shl 6;
  SDL_HAPTIC_SPRING=1 shl 7;
  SDL_HAPTIC_DAMPER=1 shl 8;
  SDL_HAPTIC_INERTIA=1 shl 9;
  SDL_HAPTIC_FRICTION=1 shl 10;
  SDL_HAPTIC_CUSTOM=1 shl 11;
  SDL_HAPTIC_GAIN=1 shl 12;
  SDL_HAPTIC_AUTOCENTER=1 shl 13;
  SDL_HAPTIC_STATUS=1 shl 14;
  SDL_HAPTIC_PAUSE=1 shl 15;

  SDL_HAPTIC_POLAR=0;
  SDL_HAPTIC_CARTESIAN=1;
  SDL_HAPTIC_SPHERICAL=2;
  SDL_HAPTIC_INFINITY=4294967295;

type
  PSDL_Haptic=Pointer;

  PSDL_HapticDirection=^TSDL_HapticDirection;
  TSDL_HapticDirection=record
    type_: Uint8;
    dir: array[0..2] of Sint32;
  end;

  PSDL_HapticConstant=^TSDL_HapticConstant;
  TSDL_HapticConstant=record
    type_: Uint16;
    direction: TSDL_HapticDirection;
    length: Uint32;
    delay,
    button,
    interval: Uint16;
    level: Sint16;
    attack_length,
    attack_level,
    fade_length,
    fade_level: Uint16;
  end;

  PSDL_HapticPeriodic=^TSDL_HapticPeriodic;
  TSDL_HapticPeriodic=record
    type_: Uint16;
    direction: TSDL_HapticDirection;
    length: Uint32;
    delay,
    button,
    interval,
    period: Uint16;
    magnitude,
    offset: Sint16;
    phase,
    attack_length,
    attack_level,
    fade_length,
    fade_level: Uint16;
  end;

  PSDL_HapticCondition=^TSDL_HapticCondition;
  TSDL_HapticCondition=record
    type_: Uint16;
    direction: TSDL_HapticDirection;
    length: Uint32;
    delay,
    button,
    interval: Uint16;
    right_sat,
    left_sat: array[0..2] of Uint16;
    right_coeff,
    left_coeff: array[0..2] of Sint16;
    deadband: array[0..2] of Uint16;
    center: array[0..2] of Sint16;
  end;

  PSDL_HapticRamp=^TSDL_HapticRamp;
  TSDL_HapticRamp=record
    type_: Uint16;
    direction: TSDL_HapticDirection;
    length: Uint32;
    delay,
    button,
    interval: Uint16;
    start,
    end_: Sint16;
    attack_length,
    attack_level,
    fade_length,
    fade_level: Uint16;
  end;

  PSDL_HapticLeftRight=^TSDL_HapticLeftRight;
  TSDL_HapticLeftRight=record
    type_: Uint16;
    length: Uint32;
    large_magnitude,
    small_magnitude: Uint16;
  end;

  PSDL_HapticCustom=^TSDL_HapticCustom;
  TSDL_HapticCustom=record
    type_: Uint16;
    direction: TSDL_HapticDirection;
    length: Uint32;
    delay,
    button,
    interval: Uint16;
    channels: Uint8;
    period,
    samples: Uint16;
    data: PUint16;
    attack_length,
    attack_level,
    fade_length,
    fade_level: Uint16;
  end;

  PSDL_HapticEffect=^TSDL_HapticEffect;
  TSDL_HapticEffect=record
    case Uint16 of
      0: (type_: Uint16);
      1: (constant: TSDL_HapticConstant);
      2: (periodic: TSDL_HapticPeriodic);
      3: (condition: TSDL_HapticCondition);
      4: (ramp: TSDL_HapticRamp);
      5: (custom: TSDL_HapticCustom);
  end;

function SDL_NumHaptics: longint; lSDL;
function SDL_HapticName(device_index: longint): pchar; lSDL;
function SDL_HapticOpen(device_index: longint): PSDL_Haptic; lSDL;
function SDL_HapticOpened(device_index: longint): longint; lSDL;
function SDL_HapticIndex(haptic: PSDL_Haptic): longint; lSDL;
function SDL_MouseIsHaptic: longint; lSDL;
function SDL_HapticOpenFromMouse: PSDL_Haptic; lSDL;
function SDL_JoystickIsHaptic(joystick: PSDL_Joystick): longint; lSDL;
function SDL_HapticOpenFromJoystick(joystick: PSDL_Joystick): PSDL_Haptic; lSDL;
procedure SDL_HapticClose(haptic: PSDL_Haptic); lSDL;
function SDL_HapticNumEffects(haptic: PSDL_Haptic): longint; lSDL;
function SDL_HapticNumEffectsPlaying(haptic: PSDL_Haptic): longint; lSDL;
function SDL_HapticQuery(haptic: PSDL_Haptic): longint; lSDL;
function SDL_HapticNumAxes(haptic: PSDL_Haptic): longint; lSDL;
function SDL_HapticEffectSupported(haptic: PSDL_Haptic;
                                   effect: PSDL_HapticEffect): longint; lSDL;
function SDL_HapticNewEffect(haptic: PSDL_Haptic;
                             effect: PSDL_HapticEffect): longint; lSDL;
function SDL_HapticUpdateEffect(haptic: PSDL_Haptic; effect: longint;
                                data: PSDL_HapticEffect): longint; lSDL;
function SDL_HapticRunEffect(haptic: PSDL_Haptic; effect: longint;
                             iterations: Uint32): longint; lSDL;
function SDL_HapticStopEffect(haptic: PSDL_Haptic;
                              effect: longint): longint; lSDL;
procedure SDL_HapticDestroyEffect(haptic: PSDL_Haptic; effect: longint); lSDL;
function SDL_HapticGetEffectStatus(haptic: PSDL_Haptic;
                                   effect: longint): longint; lSDL;
function SDL_HapticSetGain(haptic: PSDL_Haptic; gain: longint): longint; lSDL;
function SDL_HapticSetAutocenter(haptic: PSDL_Haptic;
                                 autocenter: longint): longint; lSDL;
function SDL_HapticPause(haptic: PSDL_Haptic): longint; lSDL;
function SDL_HapticUnpause(haptic: PSDL_Haptic): longint; lSDL;
function SDL_HapticStopAll(haptic: PSDL_Haptic): longint; lSDL;
function SDL_HapticRumbleSupported(haptic: PSDL_Haptic): longint; lSDL;
function SDL_HapticRumbleInit(haptic: PSDL_Haptic): longint; lSDL;
function SDL_HapticRumblePlay(haptic: PSDL_Haptic;
                              strength: single; length: Uint32): longint; lSDL;
function SDL_HapticRumbleStop(haptic: PSDL_Haptic): longint; lSDL;

//=====SDL_TOUCH=====

type
  TSDL_TouchID=Sint64;
  TSDL_FingerID=Sint64;

  PSDL_Finger=^TSDL_Finger;
  TSDL_Finger=record
    id: TSDL_FingerID;
    x, y,
    pressure: single;
  end;

const
  SDL_TOUCH_MOUSEID=Uint32(-1);

function SDL_GetNumTouchDevices: longint; lSDL;
function SDL_GetTouchDevice(index: longint): TSDL_TouchID; lSDL;
function SDL_GetNumTouchFingers(touchID: TSDL_TouchID): longint; lSDL;
function SDL_GetTouchFinger(touchID: TSDL_TouchID;
                            index: longint): PSDL_Finger; lSDL;

//======SDL_GESTURE=====

type
  TSDL_GestureID=Sint64;

function SDL_RecordGesture(touchId: TSDL_TouchID): longint; lSDL;
function SDL_SaveAllDollarTemplates(src: PSDL_RWops): longint; lSDL;
function SDL_SaveDollarTemplate(gestureId: TSDL_GestureID;
                                src: PSDL_RWops): longint; lSDL;
function SDL_LoadDollarTemplates(touchId: TSDL_TouchID;
                                 src: PSDL_RWops): longint; lSDL;

//=====SDL_EVENTS=====

const
  SDL_RELEASED=0;
  SDL_PRESSED=1;

  SDL_FIRSTEVENT=0;
  SDL_QUITEV=$100;

  SDL_APP_TERMINATING=$101;
  SDL_APP_LOWMEMORY=$102;
  SDL_APP_WILLENTERBACKGROUND=$103;
  SDL_APP_DIDENTERBACKGROUND=$104;
  SDL_APP_WILLENTERFOREGROUND=$105;
  SDL_APP_DIDENTERFOREGROUND=$106;

  SDL_WINDOWEVENT=$200;
  SDL_SYSWMEVENT=$201;

  SDL_KEYDOWN=$300;
  SDL_KEYUP=$301;
  SDL_TEXTEDITING=$302;
  SDL_TEXTINPUT=$303;

  SDL_MOUSEMOTION=$400;
  SDL_MOUSEBUTTONDOWN=$401;
  SDL_MOUSEBUTTONUP=$402;
  SDL_MOUSEWHEEL=$403;

  SDL_JOYAXISMOTION=$600;
  SDL_JOYBALLMOTION=$601;
  SDL_JOYHATMOTION=$602;
  SDL_JOYBUTTONDOWN=$603;
  SDL_JOYBUTTONUP=$604;
  SDL_JOYDEVICEADDED=$605;
  SDL_JOYDEVICEREMOVED=$606;

  SDL_CONTROLLERAXISMOTION=$650;
  SDL_CONTROLLERBUTTONDOWN=$651;
  SDL_CONTROLLERBUTTONUP=$652;
  SDL_CONTROLLERDEVICEADDED=$653;
  SDL_CONTROLLERDEVICEREMOVED=$654;
  SDL_CONTROLLERDEVICEREMAPPED=$655;

  SDL_FINGERDOWN=$700;
  SDL_FINGERUP=$701;
  SDL_FINGERMOTION=$702;

  SDL_DOLLARGESTURE=$800;
  SDL_DOLLARRECORD=$801;
  SDL_MULTIGESTURE=$802;

  SDL_CLIPBOARDUPDATE=$900;

  SDL_DROPFILE=$1000;
  SDL_RENDER_TARGETS_RESET=$2000;

  SDL_USEREVENT=$8000;

  SDL_LASTEVENT=$FFFF;

  SDL_QUERY=-1;
  SDL_IGNORE=0;
  SDL_DISABLE=0;
  SDL_ENABLE=1;

type
  TSDL_EventType=longword;

  TSDL_CommonEvent=record
    type_,
    timestamp: Uint32;
  end;

  TSDL_WindowEvent=record
    type_,
    timestamp,
    windowID: Uint32;
    event,
    padding1,
    padding2,
    padding3: UInt8;
    data1,
    data2: Sint32;
  end;

  TSDL_KeyboardEvent=record
    type_,
    timestamp,
    windowID: Uint32;
    state,
    repeat_,
    padding2,
    padding3: UInt8;
    keysym: TSDL_KeySym;
  end;

const
  SDL_TEXTEDITINGEVENT_TEXT_SIZE=32;

type
  TSDL_TextEditingEvent=record
    type_,
    timestamp,
    windowID: Uint32;
    text: array[0..SDL_TEXTEDITINGEVENT_TEXT_SIZE-1] of char;
    start,
    length: Sint32;
  end;

const
  SDL_TEXTINPUTEVENT_TEXT_SIZE=32;

type
  TSDL_TextInputEvent=record
    type_,
    timestamp,
    windowID: Uint32;
    text: array[0..SDL_TEXTINPUTEVENT_TEXT_SIZE-1] of char;
  end;

  TSDL_MouseMotionEvent=record
    type_,
    timestamp,
    windowID,
    which,
    state: Uint32;
    x, y,
    xrel, yrel: Sint32;
  end;

  TSDL_MouseButtonEvent=record
    type_,
    timestamp,
    windowID,
    which: Uint32;
    button,
    state,
    clicks,
    padding1: UInt8;
    x, y: Sint32;
  end;

  TSDL_MouseWheelEvent=record
    type_,
    timestamp,
    windowID,
    which: Uint32;
    x, y: Sint32;
  end;

  TSDL_JoyAxisEvent=record
    type_,
    timestamp: Uint32;
    which: TSDL_JoystickID;
    axis,
    padding1,
    padding2,
    padding3: UInt8;
    value: Sint16;
    padding4: UInt16;
  end;

  TSDL_JoyBallEvent=record
    type_,
    timestamp: Uint32;
    which: TSDL_JoystickID;
    ball,
    padding1,
    padding2,
    padding3: UInt8;
    xrel, yrel: Sint16;
  end;

  TSDL_JoyHatEvent=record
    type_,
    timestamp: Uint32;
    which: TSDL_JoystickID;
    hat,
    value,
    padding1,
    padding2: UInt8;
  end;

  TSDL_JoyButtonEvent=record
    type_,
    timestamp: Uint32;
    which: TSDL_JoystickID;
    button,
    state,
    padding1,
    padding2: UInt8;
  end;

  TSDL_JoyDeviceEvent=record
    type_,
    timestamp: Uint32;
    which: Sint32;
  end;

  TSDL_ControllerAxisEvent=record
    type_,
    timestamp: Uint32;
    which: TSDL_JoystickID;
    axis,
    padding1,
    padding2,
    padding3: UInt8;
    value: Sint16;
    padding4: UInt16;
  end;

  TSDL_ControllerButtonEvent=record
    type_,
    timestamp: Uint32;
    which: TSDL_JoystickID;
    button,
    state,
    padding1,
    padding2: UInt8;
  end;

  TSDL_ControllerDeviceEvent=record
    type_,
    timestamp: Uint32;
    which: Sint32;
  end;

  TSDL_TouchFingerEvent=record
    type_,
    timestamp: Uint32;
    touchId: TSDL_TouchID;
    fingerId: TSDL_FingerID;
    x, y,
    dx, dy,
    pressure: single;
  end;

  TSDL_MultiGestureEvent=record
    type_,
    timestamp: Uint32;
    touchId: TSDL_TouchID;
    dTheta,
    dDist,
    x, y: single;
    numFingers,
    padding: UInt16;
  end;

  TSDL_DollarGestureEvent=record
    type_,
    timestamp: Uint32;
    touchId: TSDL_TouchID;
    gestureId: TSDL_GestureID;
    numFingers: Uint32;
    error,
    x, y: single;
  end;

  TSDL_DropEvent=record
    type_,
    timestamp: Uint32;
    file_: pchar;
  end;

  TSDL_QuitEvent=record
    type_,
    timestamp: Uint32;
  end;

  TSDL_OSEvent=record
    type_,
    timestamp: Uint32;
  end;

  TSDL_UserEvent=record
    type_,
    timestamp,
    windowID: Uint32;
    code: Sint32;
    data1,
    data2: pointer;
  end;

  TSDL_SysWMEvent=record
    type_,
    timestamp: Uint32;
    msg: pointer;
  end;

  PSDL_Event=^TSDL_Event;
  TSDL_Event=record
    case Uint32 of
      0:                                 (type_: Uint32);
      1:                                 (common: TSDL_CommonEvent);
      SDL_WINDOWEVENT:                   (window: TSDL_WindowEvent);
      SDL_KEYUP,
      SDL_KEYDOWN:                       (key: TSDL_KeyboardEvent);
      SDL_TEXTEDITING:                   (edit: TSDL_TextEditingEvent);
      SDL_TEXTINPUT:                     (text: TSDL_TextInputEvent);
      SDL_MOUSEMOTION:                   (motion: TSDL_MouseMotionEvent);
      SDL_MOUSEBUTTONUP,
      SDL_MOUSEBUTTONDOWN:               (button: TSDL_MouseButtonEvent);
      SDL_MOUSEWHEEL:                    (wheel: TSDL_MouseWheelEvent);
      SDL_JOYAXISMOTION:                 (jaxis: TSDL_JoyAxisEvent);
      SDL_JOYBALLMOTION:                 (jball: TSDL_JoyBallEvent);
      SDL_JOYHATMOTION:                  (jhat: TSDL_JoyHatEvent);
      SDL_JOYBUTTONDOWN,
      SDL_JOYBUTTONUP:                   (jbutton: TSDL_JoyButtonEvent);
      SDL_JOYDEVICEADDED,
      SDL_JOYDEVICEREMOVED:              (jdevice: TSDL_JoyDeviceEvent);
      SDL_CONTROLLERAXISMOTION:          (caxis: TSDL_ControllerAxisEvent);
      SDL_CONTROLLERBUTTONUP,
      SDL_CONTROLLERBUTTONDOWN:          (cbutton: TSDL_ControllerButtonEvent);
      SDL_CONTROLLERDEVICEADDED,
      SDL_CONTROLLERDEVICEREMOVED,
      SDL_CONTROLLERDEVICEREMAPPED:      (cdevice: TSDL_ControllerDeviceEvent);
      SDL_QUITEV:                        (quit: TSDL_QuitEvent);
      SDL_USEREVENT:                     (user: TSDL_UserEvent);
      SDL_SYSWMEVENT:                    (syswm: TSDL_SysWMEvent);
      SDL_FINGERDOWN,
      SDL_FINGERUP,
      SDL_FINGERMOTION:                  (tfinger: TSDL_TouchFingerEvent);
      SDL_MULTIGESTURE:                  (mgesture: TSDL_MultiGestureEvent);
      SDL_DOLLARGESTURE,SDL_DOLLARRECORD:(dgesture: TSDL_DollarGestureEvent);
      SDL_DROPFILE:                      (drop: TSDL_DropEvent);
      SDL_LASTEVENT+1: (padding: array[0..55] of Uint8);
  end;

  TSDL_eventaction=(
    SDL_ADDEVENT,
    SDL_PEEKEVENT,
    SDL_GETEVENT);

  PSDL_EventFilter=^TSDL_EventFilter;
  TSDL_EventFilter=function(userdata: pointer;
                            event: PSDL_Event): longint; cdecl;

function SDL_QuitRequested: boolean; inline;

procedure SDL_PumpEvents; lSDL;
function SDL_PeepEvents(events: PSDL_Event; numevents: longint;
                        action: TSDL_eventaction;
                        minType, maxType: Uint32): longint; lSDL;
function SDL_HasEvent(type_: Uint32): SDL_bool; lSDL;
function SDL_HasEvents(minType, maxType: Uint32): SDL_bool; lSDL;
procedure SDL_FlushEvent(type_: Uint32); lSDL;
procedure SDL_FlushEvents(minType, maxType: Uint32); lSDL;
function SDL_PollEvent(event: PSDL_Event): longint; lSDL;
function SDL_WaitEvent(event: PSDL_Event): longint; lSDL;
function SDL_WaitEventTimeout(event: PSDL_Event;
                              timeout: longint): longint; lSDL;
function SDL_PushEvent(event: PSDL_Event): longint; lSDL;
procedure SDL_SetEventFilter(filter: TSDL_EventFilter;
                             userdata: pointer); lSDL;
function SDL_GetEventFilter(filter: PSDL_EventFilter;
                            userdata: ppointer): SDL_bool; lSDL;
procedure SDL_AddEventWatch(filter: TSDL_EventFilter; userdata: pointer); lSDL;
procedure SDL_DelEventWatch(filter: TSDL_EventFilter; userdata: pointer); lSDL;
procedure SDL_FilterEvents(filter: TSDL_EventFilter; userdata: pointer); lSDL;
function SDL_EventState(type_: Uint32; state: longint): Uint8; lSDL;
function SDL_GetEventState(type_: Uint32): Uint8; inline;
function SDL_RegisterEvents(numevents: longint): Uint32; lSDL;

//=====SDL_SYSTEM=====
{$IFDEF WINDOWS}
function SDL_Direct3D9GetAdapterIndex(displayIndex: longint): longint; lSDL;
function SDL_RenderGetD3D9Device(renderer: PSDL_Renderer): pointer; lSDL;
procedure SDL_DXGIGetOutputInfo(displayIndex: longint;
                                adapterIndex, outputIndex: plongint); lSDL;
{$ENDIF}

{$IFDEF IPHONEOS}
function SDL_iPhoneSetAnimationCallback(window: PSDL_Window;
                                        interval: longint;
                                        callback,
                                        callbackParam: pointer); lSDL;
procedure SDL_iPhoneSetEventPump(enabled: SDL_bool); lSDL;
{$ENDIF}

{$IFDEF ANDROID}
const
  SDL_ANDROID_EXTERNAL_STORAGE_READ = $01;
  SDL_ANDROID_EXTERNAL_STORAGE_WRITE = $02;

function SDL_AndroidGetJNIEnv: pointer; lSDL;
function SDL_AndroidGetActivity: pointer; lSDL;
function SDL_AndroidGetInternalStoragePath: pchar; lSDL;
function SDL_AndroidGetExternalStorageState: longint; lSDL;
function SDL_AndroidGetExternalStoragePath: pchar; lSDL;
{$ENDIF}

implementation

//=====SDL_STDINC=====

function SDL_iconv_utf8_locale(S: pchar): pchar; inline;
begin
  SDL_iconv_utf8_locale:=SDL_iconv_string('', 'UTF-8', S, SDL_strlen(S)+1);
end;

function SDL_iconv_utf8_ucs2(S: pchar): pchar; inline;
begin
  SDL_iconv_utf8_ucs2:=SDL_iconv_string('UCS-2-INTERNAL', 'UTF-8',
    S, SDL_strlen(S)+1);
end;

function SDL_iconv_utf8_ucs4(S: pchar): pchar; inline;
begin
  SDL_iconv_utf8_ucs4:=SDL_iconv_string('UCS-4-INTERNAL', 'UTF-8',
    S, SDL_strlen(S)+1);
end;

//=====SDL_BITS=====

function SDL_MostSignificantBitIndex32(x: Uint32): longint; inline;
begin
  if x=0 then
    exit(-1)
  else
    SDL_MostSignificantBitIndex32:=BsrDword(x);
end;

//=====SDL_VERSION=====

procedure SDL_VERSION(x: PSDL_Version); inline;
begin
  x^.major:=SDL_MAJOR_VERSION;
  x^.minor:=SDL_MINOR_VERSION;
  x^.patch:=SDL_PATCHLEVEL;
end;

function SDL_VERSIONNUM(X, Y, Z: longint): longint; inline;
begin
  SDL_VERSIONNUM:=X*1000+Y*100+Z;
end;

function SDL_COMPILEDVERSION: longint; inline;
begin
  SDL_COMPILEDVERSION:=SDL_VERSIONNUM(SDL_MAJOR_VERSION,
    SDL_MINOR_VERSION, SDL_PATCHLEVEL);
end;

function SDL_VERSION_ATLEAST(X, Y, Z: longint): boolean; inline;
begin
  SDL_VERSION_ATLEAST:=SDL_COMPILEDVERSION>=SDL_VERSIONNUM(X, Y, Z);
end;

//=====SDL_TIMER=====

function SDL_TICKS_PASSED(a, b: Uint32): SDL_bool; inline;
begin
  SDL_TICKS_PASSED:=(b-a)<=0;
end;

//=====SDL_MUTEX=====

function SDL_MutexP(mutex: PSDL_Mutex): longint; inline;
begin
  SDL_MutexP:=SDL_LockMutex(mutex);
end;

function SDL_MutexV(mutex: PSDL_Mutex): longint; inline;
begin
  SDL_MutexV:=SDL_UnlockMutex(mutex);
end;

//=====SDL_RWOPS=====

function SDL_RWsize(ctx: PSDL_RWops): Sint64; inline;
begin
  SDL_RWsize:=ctx^.size(ctx);
end;

function SDL_RWseek(ctx: PSDL_RWops; offset: Sint64;
                    whence: longint): Sint64; inline;
begin
  SDL_RWseek:=ctx^.seek(ctx,offset,whence);
end;

function SDL_RWtell(ctx: PSDL_RWops): Sint64;
begin
  SDL_RWtell:=ctx^.seek(ctx, 0, RW_SEEK_CUR);
end;

function SDL_RWread(ctx: PSDL_RWops; ptr: pointer; size: longword;
                    n: longword): longword; inline;
begin
  SDL_RWread:=ctx^.read(ctx, ptr, size, n);
end;

function SDL_RWwrite(ctx: PSDL_RWops; ptr: pointer; size: longword;
                     n: longword): longword; inline;
begin
  SDL_RWwrite:=ctx^.write(ctx, ptr, size, n);
end;

function SDL_RWclose(ctx: PSDL_RWops): longint; inline;
begin
  SDL_RWclose:=ctx^.close(ctx);
end;

//=====SDL_AUDIO=====

function SDL_AUDIO_BITSIZE(x: Uint16): Uint16; inline;
begin
  SDL_AUDIO_BITSIZE:=x and SDL_AUDIO_MASK_BITSIZE;
end;

function SDL_AUDIO_ISFLOAT(x: Uint16): boolean; inline;
begin
  SDL_AUDIO_ISFLOAT:=(x and SDL_AUDIO_MASK_DATATYPE)>0;
end;

function SDL_AUDIO_ISBIGENDIAN(x: Uint16): boolean; inline;
begin
  SDL_AUDIO_ISBIGENDIAN:=(x and SDL_AUDIO_MASK_ENDIAN)>0;
end;

function SDL_AUDIO_ISSIGNED(x: Uint16): boolean; inline;
begin
  SDL_AUDIO_ISSIGNED:=(x and SDL_AUDIO_MASK_SIGNED)>0;
end;

function SDL_AUDIO_ISINT(x: Uint16): boolean; inline;
begin
  SDL_AUDIO_ISINT:=not SDL_AUDIO_ISFLOAT(x);
end;

function SDL_AUDIO_ISLITTLEENDIAN(x: Uint16): boolean; inline;
begin
  SDL_AUDIO_ISLITTLEENDIAN:=not SDL_AUDIO_ISBIGENDIAN(x);
end;

function SDL_AUDIO_ISUNSIGNED(x: Uint16): boolean; inline;
begin
  SDL_AUDIO_ISUNSIGNED:=not SDL_AUDIO_ISSIGNED(x);
end;

function SDL_LoadWAV(file_: pchar;
                     spec: PSDL_AudioSpec;
                     audio_buf: ppbyte;
                     audio_len: PUint32): PSDL_AudioSpec; inline;
begin
  SDL_LoadWAV:=SDL_LoadWAV_RW(SDL_RWFromFile(file_, 'r'), 1,
                              spec, audio_buf, audio_len);
end;
//=====SDL_PIXELS=====

function SDL_PIXELFLAG(X: Uint32): Uint32; inline;
begin
  SDL_PIXELFLAG:=(X shr 28) and $0F;
end;

function SDL_PIXELTYPE(X: Uint32): Uint32; inline;
begin
  SDL_PIXELTYPE:=(X shr 24) and $0F;
end;

function SDL_PIXELORDER(X: Uint32): Uint32; inline;
begin
  SDL_PIXELORDER:=(X shr 20) and $0F;
end;

function SDL_PIXELLAYOUT(X: Uint32): Uint32; inline;
begin
  SDL_PIXELLAYOUT:=(X shr 16) and $0F;
end;

function SDL_BITSPERPIXEL(X: Uint32): Uint32; inline;
begin
  SDL_BITSPERPIXEL:=(X shr 8) and $FF;
end;

function SDL_BYTESPERPIXEL(X: Uint32): Uint32; inline;
begin
  if SDL_ISPIXELFORMAT_FOURCC(X) then
    begin
      if (X=SDL_PIXELFORMAT_YUY2) or
         (X=SDL_PIXELFORMAT_UYVY) or
         (X=SDL_PIXELFORMAT_YVYU) then
        SDL_BYTESPERPIXEL:=2
      else
        SDL_BYTESPERPIXEL:=1;
    end
  else
    SDL_BYTESPERPIXEL:=(X shr 0) and $FF;
end;

function SDL_ISPIXELFORMAT_INDEXED(format: Uint32): boolean; inline;
begin
  SDL_ISPIXELFORMAT_INDEXED:=(not SDL_ISPIXELFORMAT_FOURCC(format)) and
    ((SDL_PIXELTYPE(format)=SDL_PIXELTYPE_INDEX1) or
     (SDL_PIXELTYPE(format)=SDL_PIXELTYPE_INDEX4) or
     (SDL_PIXELTYPE(format)=SDL_PIXELTYPE_INDEX8));
end;

function SDL_ISPIXELFORMAT_ALPHA(format: Uint32): boolean; inline;
begin
  SDL_ISPIXELFORMAT_ALPHA:=(not SDL_ISPIXELFORMAT_FOURCC(format)) and
    ((SDL_PIXELORDER(format)=SDL_PACKEDORDER_ARGB) or
     (SDL_PIXELORDER(format)=SDL_PACKEDORDER_RGBA) or
     (SDL_PIXELORDER(format)=SDL_PACKEDORDER_ABGR) or
     (SDL_PIXELORDER(format)=SDL_PACKEDORDER_BGRA));
end;

function SDL_ISPIXELFORMAT_FOURCC(format: Uint32): boolean; inline;
begin
  SDL_ISPIXELFORMAT_FOURCC:=(format and SDL_PIXELFLAG(format))<>1;
end;

//=====SDL_RECT=====

function SDL_RectEmpty(const x: PSDL_Rect): SDL_bool; inline;
begin
  SDL_RectEmpty:=(x=NIL) or (x^.w<=0) or (x^.h<=0);
end;

function SDL_RectEquals(const a, b: PSDL_Rect): SDL_bool; inline;
begin
  SDL_RectEquals:=(a<>NIL) and (b<>NIL) and
                  (a^.x=b^.x) and (a^.y=b^.y) and (a^.w=b^.w) and (a^.h=b^.h);
end;

//=====SDL_SURFACE=====

function SDL_MUSTLOCK(S: PSDL_Surface): boolean; inline;
begin
  SDL_MUSTLOCK:=(S^.flags and SDL_RLEACCEL)<>0;
end;

function SDL_LoadBMP(file_: pchar): PSDL_Surface; inline;
begin
  SDL_LoadBMP:=SDL_LoadBMP_RW(SDL_RWFromFile(file_, 'r'), 1);
end;

function SDL_SaveBMP(surface: PSDL_Surface; file_: pchar): longint; inline;
begin
  SDL_SaveBMP:=SDL_SaveBMP_RW(surface, SDL_RWFromFile(file_, 'w'), 1);
end;

function SDL_BlitSurface(src: PSDL_Surface; const srcrect: PSDL_Rect;
                        dst: PSDL_Surface; dstrect: PSDL_Rect): longint; inline;
begin
  SDL_BlitSurface:=SDL_UpperBlit(src, srcrect, dst, dstrect);
end;

function SDL_BlitScaled(src: PSDL_Surface; const srcrect: PSDL_Rect;
                        dst: PSDL_Surface; dstrect: PSDL_Rect): longint; inline;
begin
  SDL_BlitScaled:=SDL_UpperBlitScaled(src, srcrect, dst, dstrect);
end;

//=====SDL_GAMECONTROLLER=====

function SDL_GameControllerAddMappingsFromFile(file_: pchar): longint; inline;
begin
  SDL_GameControllerAddMappingsFromFile:=SDL_GameControllerAddMappingsFromRW(
    SDL_RWFromFile(file_, 'r'), 1);
end;

//=====SDL_EVENTS=====

function SDL_QuitRequested: boolean; inline;
begin
  SDL_PumpEvents;
  SDL_QuitRequested:=SDL_PeepEvents(NIL, 0,
    SDL_PEEKEVENT, SDL_QUITEV, SDL_QUITEV)>0;
end;

function SDL_GetEventState(type_: Uint32): Uint8; inline;
begin
  SDL_GetEventState:=SDL_EventState(type_, SDL_QUERY);
end;

end.
