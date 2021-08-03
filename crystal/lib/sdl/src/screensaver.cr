module SDL
  module Screensaver
    def self.enabled?
      LibSDL.is_screen_saver_enabled() == 1
    end

    def self.enable
      LibSDL.enable_screen_saver()
    end

    def self.disable
      LibSDL.disable_screen_saver()
    end
  end
end
