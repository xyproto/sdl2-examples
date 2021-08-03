lib LibSDL
  enum PowerState
    UNKNOWN
    ON_BATTERY
    NO_BATTERY
    CHARGING
    CHARGED
  end

  fun get_power_info = SDL_GetPowerInfo(secs : Int*, pct : Int*) : PowerState
end
