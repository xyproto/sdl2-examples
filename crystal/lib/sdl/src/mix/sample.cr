module SDL
  module Mix
    class Sample
      def self.decoder_count
        LibMix.get_num_chunk_decoders
      end

      def self.decoder_name(index)
        LibMix.get_chunk_decoder(index)
      end

      def initialize(filename)
        @rwops = SDL::RWops.new(filename, "rb")
        @sample = LibMix.load_wav_rw(@rwops, 1)
        raise SDL::Error.new("Mix_LoadWAV_RW") unless @sample
      end

      def finalize
        LibMix.free_chunk(self)
      end

      def to_unsafe
        @sample
      end
    end
  end
end
