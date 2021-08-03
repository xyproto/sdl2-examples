module SDL
  @[Extern]
  struct Point
    property x : Int32
    property y : Int32

    macro [](x, y)
      SDL::Point.new({{x}}, {{y}})
    end

    def initialize(@x, @y)
    end
  end

  @[Extern]
  struct Rect
    property x : Int32
    property y : Int32
    property w : Int32
    property h : Int32

    macro [](x, y, w, h)
      SDL::Rect.new({{x}}, {{y}}, {{w}}, {{h}})
    end

    def initialize(@x, @y, @w, @h)
    end
  end
end
