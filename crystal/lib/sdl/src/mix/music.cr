module SDL
  module Mix
    class Music
      @music : Pointer(LibMix::Music)

      def initialize(filename, type : Type? = nil)
        @rwops = SDL::RWops.new(filename, "rb")
        if type
          @music = load_music_type(@rwops, type)
        else
          @music = load_music(@rwops)
        end
      end

      def play(repeats = -1)
        LibMix.play_music(self, repeats)
      end

      def pause
        LibMix.pause_music
      end

      def resume
        LibMix.resume_music
      end

      def stop
        LibMix.halt_music
      end

      def playing?
        LibMix.music_playing == 1
      end

      def paused?
        LibMix.music_paused == 1
      end

      def rewind
        LibMix.rewind_music
      end

      def fade_in(loops = -1, msec = 1000)
        LibMix.fade_in_music(self, loops, msec)
      end

      def fade_out(msec = 1000)
        LibMix.fade_out_music(msec)
      end

      def volume=(volume)
        LibMix.music_volume(volume > MAX_VOLUME ? MAX_VOLUME : volume)
      end

      def volume
        LibMix.music_volume(-1)
      end

      def finalize
        LibMix.free_music(self)
      end

      def to_unsafe
        @music
      end

      private def load_music(rwops)
        audio = LibMix.load_mus_rw(rwops, 1)
        raise SDL::Error.new("Mix_LoadMUS_RW") unless audio
        audio
      end

      private def load_music_type(rwops, type)
        audio = LibMix.load_mus_type_rw(rwops, type.to_s, 1)
        raise SDL::Error.new("Mix_LoadMUSType_RW") unless audio
        audio
      end
    end
  end
end
