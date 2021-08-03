lib LibSDL
  alias AudioFormat = UInt16
  alias AudioDeviceID = UInt32

  AUDIO_U8 = 0x0008
  AUDIO_S8 = 0x8008

  AUDIO_U16LSB = 0x0010
  AUDIO_S16LSB = 0x8010
  AUDIO_U16MSB = 0x1010
  AUDIO_S16MSB = 0x9010
  AUDIO_U16 = AUDIO_U16LSB
  AUDIO_S16 = AUDIO_S16LSB

  AUDIO_S32LSB = 0x8020
  AUDIO_S32MSB = 0x9020
  AUDIO_S32 = AUDIO_S32LSB

  AUDIO_F32LSB = 0x8120
  AUDIO_F32MSB = 0x9120
  AUDIO_F32 = AUDIO_F32LSB

  # LITTLE ENDIAN
  AUDIO_U16SYS = AUDIO_U16LSB
  AUDIO_S16SYS = AUDIO_S16LSB
  AUDIO_S32SYS = AUDIO_S32LSB
  AUDIO_F32SYS = AUDIO_F32LSB

  # BIG ENDIAN
  #AUDIO_U16SYS = AUDIO_U16MSB
  #AUDIO_S16SYS = AUDIO_S16MSB
  #AUDIO_S32SYS    AUDIO_S32MSB
  #AUDIO_F32SYS    AUDIO_F32MSB

  AUDIO_ALLOW_FREQUENCY_CHANGE = 0x00000001
  AUDIO_ALLOW_FORMAT_CHANGE    = 0x00000002
  AUDIO_ALLOW_CHANNELS_CHANGE  = 0x00000004
  AUDIO_ALLOW_ANY_CHANGE       = SDL_AUDIO_ALLOW_FREQUENCY_CHANGE | SDL_AUDIO_ALLOW_FORMAT_CHANGE | SDL_AUDIO_ALLOW_CHANNELS_CHANGE

  # (userdata, stream, size)
  alias AudioCallback = (Void*, UInt8*, Int) -> Void

  struct AudioSpec
	freq : Int
	format : AudioFormat
	channels : UInt8
	silence : UInt8
	samples : UInt16
	padding : UInt16
	size : UInt32
    callback : AudioCallback*
	userdata : Void*
  end

  # (cvt, format)
  alias AudioFilter = (AudioCVT*, AudioFormat) -> Void

  @[Packed]
  struct AudioCVT
	needed : Int
	src_format : AudioFormat
	dst_format : AudioFormat
	rate_incr : Double
	buf : UInt8*
	len : Int
	len_cvt : Int
	len_mult : Int
	len_ratio : Double
    filters : StaticArray(AudioFilter*, 10)
	filter_index : Int
  end

  enum AudioStatus
	STOPPED = 0
	PLAYING= 1
    PAUSED
  end

  fun get_num_audio_drivers = SDL_GetNumAudioDrivers() : Int
  fun get_audio_driver = SDL_GetAudioDriver(index : Int) : Char*
  fun audio_init = SDL_AudioInit(driver_name : Char*) : Int
  fun audio_quit = SDL_AudioQuit()
  fun get_current_audio_driver = SDL_GetCurrentAudioDriver() : Char*
  fun open_audio = SDL_OpenAudio(desierd : AudioSpec*, obtained : AudioSpec*) : Int

  fun get_num_audio_devices = SDL_GetNumAudioDevices(iscapture : Int) : Int
  fun get_audio_device_name = SDL_GetAudioDeviceName(index : Int, iscapture : Int) : Char*

  fun open_audio_device = SDL_OpenAudioDevice(device : Char*, iscapture : Int, desired : AudioSpec*, obtained : AudioSpec*, allowed_changes : Int) : AudioDeviceID

  fun get_audio_status = SDL_GetAudioStatus() : AudioStatus
  fun get_audio_device_status = SDL_GetAudioDeviceStatus(dev : AudioDeviceID) : AudioStatus

  fun pause_audio = SDL_PauseAudio(pause_on : Int)
  fun pause_audio_device = SDL_PauseAudioDevice(dev : AudioDeviceID, pause_on : Int)

  fun load_wav_rw = SDL_LoadWAV_RW(src : RWops*, freesrc : Int, spec : AudioSpec*, audio_buf : UInt8**, audio_len : UInt32*)
  # SDL_LoadWAV(file, spec, audio_buf, audio_len) SDL_LoadWAV_RW(SDL_RWFromFile(file, "rb"),1, spec,audio_buf,audio_len)
  fun free_wav = SDL_FreeWAV(audio_buf : UInt8)

  fun build_audio_cvt = SDL_BuildAudioCVT(
    cvt : AudioCVT*,
    src_format : AudioFormat, src_channels : UInt8, src_rate : Int,
    dst_format : AudioFormat, dst_channels : UInt8, dst_rate : Int
  ) : Int
  fun convert_audio = SDL_ConvertAudio(cvt : AudioCVT*) : Int

  Mix_MAXVOLUME = 128
  fun mix_audio = SDL_MixAudio(dst : UInt8*, src : UInt8*, len : UInt32, volume : Int)
  fun mix_audio_format = SDL_MixAudioFormat(dst : UInt8*, src : UInt8*, format : AudioFormat, len : UInt32, volume : Int)

  fun lock_audio = SDL_LockAudio()
  fun lock_audio_device = SDL_LockAudioDevice(dev : AudioDeviceID)
  fun unlock_audio = SDL_UnlockAudio()
  fun unlock_audio_device = SDL_UnlockAudioDevice(dev : AudioDeviceID)

  fun close_audio = SDL_CloseAudio()
  fun close_audio = SDL_CloseAudioDevice(dev : AudioDeviceID)
end
