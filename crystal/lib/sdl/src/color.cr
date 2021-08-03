module SDL
  @[Extern]
  struct Color
    property r : UInt8
    property g : UInt8
    property b : UInt8
    property a : UInt8

    # Creates a RGB(A) `Color`. For example `Color[0, 0, 0]` is opaque black, whereas
    # `Color[255, 0, 0, 128]` is half transparent red.
    macro [](r, g, b, a = 255)
      SDL::Color.new({{r}}, {{g}}, {{b}}, {{a}})
    end

    # Creates a `Color` of a single intensity. For example `Color[255]` is white and
    # equivalent to `Color[255, 255, 255]`.
    macro [](rgb, a = 255)
      SDL::Color.new({{rgb}}, {{rgb}}, {{rgb}}, {{a}})
    end

    def initialize(r, g, b, a = 255_u8)
      @r = r.to_u8
      @g = g.to_u8
      @b = b.to_u8
      @a = a.to_u8
    end

    def r
      @r.to_i32
    end

    def g
      @g.to_i32
    end

    def b
      @b.to_i32
    end

    def a
      @a.to_i32
    end
  end
end
