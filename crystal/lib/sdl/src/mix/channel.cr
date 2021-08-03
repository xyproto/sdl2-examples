require "./sample"

module SDL
  module Mix
    class Channel
      ANY = new(-1)

      @@channel_count = 8
      @@reserved_count = 0

      property id : Int32

      def initialize(@id)
      end

      def self.allocate_channels(count)
        @@channel_count = count
        LibMix.allocate_channels(count)
      end

      def self.reserve_channels(count)
        @@reserved_count = count
        LibMix.reserve_channels(count)
      end

      def self.channels
        @@channel_count
      end

      def self.reserved
        @@reserved_count
      end

      def self.finished(func)
        LibMix.cb_channel_finished(func)
      end

      {% for method in %w(play fade_in fade_out resume expire volume paused_count) %}
        def self.{{method.id}}(*args)
          ANY.{{method.id}}(*args)
        end
      {% end %}

      def self.volume=(value)
        ANY.volume = value
      end

      def play(sample : Sample, repeats = 0)
        LibMix.play_channel(id, sample, repeats)
      end

      def play(sample : Sample, repeats = 0, ticks = -1)
        LibMix.play_channel_timed(id, sample, repeats, ticks)
      end

      def fade_in(sample : Sample, loops = 0, ms = 1000, ticks = -1)
        LibMix.fade_in_channel(id, sample, loops, ms, ticks)
      end

      def fade_out(ms = 1000)
        LibMix.fade_out_channel(id, ms)
      end

      def expire
        LibMix.channel_expire(id, ticks)
      end

      def fading?
        LibMix.fading? id
      end

      def paused?
        LibMix.channel_paused(id) == 1
      end

      def volume=(volume)
        LibMix.channel_volume(id, volume > MAX_VOLUME ? MAX_VOLUME : volume)
      end

      def volume
        LibMix.channel_volume(id, -1)
      end
    end
  end
end
