require "./lib_mix"
require "./sdl"
require "./mix/channel"
require "./mix/music"

module SDL
  module Mix
    MAX_VOLUME = LibMix::MIN_MAX_VOLUME
    DEFAULT_FORMAT = LibMix::Mix_DEFAULT_FORMAT

    alias Init = LibMix::Init

    enum Type
      AIFF
      FLAC
      MIDI
      MOD
      MP3
      OGG
      VOC
      WAV
    end

    # Loads support for audio decoders. `#quit` should be called during
    # application cleanup.
    def self.init(flags : Init)
      ret = LibMix.init(flags)
      unless (ret & flags.value) == flags.value
        raise SDL::Error.new("Mix_Init failed to init #{flags}")
      end
    end

    def self.quit
      LibMix.quit
    end

    # This is required to initialize SDL_Mixer. It must be called before using
    # any other function, but AFTER SDL has been initialized.
    def self.open(freq = 44100, format = DEFAULT_FORMAT, channels = 2, sample_size = 2048)
      ret = LibMix.open_audio(freq, format, channels, sample_size)
      raise SDL::Error.new("Mix_OpenAudio") unless ret == 0
      ret
    end

    def self.query_spec(freq = 44100, format = 0, channels = 2)
      audio_open_count = LibMix.query_spec(freq, format, channels)
      raise SDL::Error.new("Mix_QuerySpec: #{LibMix.get_error}") if audio_open_count < 1
      audio_open_count
    end

    def self.close
      LibMix.close_audio
    end
  end
end
