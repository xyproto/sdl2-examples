require "./lib_img"
require "./sdl"

module SDL
  module IMG
    # :nodoc:
    alias Error = SDL::Error

    alias Init = LibIMG::Init

    enum Type
      BMP
      CUR
      GIF
      ICO
      JPG
      LBM
      PCX
      PNG
      PNM
      TGA
      TIF
      XCF
      XPM
      XV
      WEBP
    end

    def self.init(flags : Init)
      ret = LibIMG.init(flags)
      unless (ret & flags.value) == flags.value
        raise SDL::Error.new("IMG_Init failed to init #{flags}")
      end
    end

    def self.quit
      LibIMG.quit
    end

    # Loads the image from *path* using the optional *type* hint as a
    # `SDL::Surface`.
    def self.load(path, type : Type? = nil)
      SDL::RWops.open(path, "rb") do |rwops|
       if type
          surface = LibIMG.load_typed_rw(rwops, 1, type.to_s)
          raise Error.new("IMG_LoadTyped_RW") unless surface
        else
          surface = LibIMG.load_rw(rwops, 1)
          raise Error.new("IMG_Load_RW") unless surface
        end
        SDL::Surface.new(surface)
      end
    end

    # Loads the image from *path* using the optional *type* hint as a
    # `SDL::Texture` for *renderer*.
    def self.load(path, renderer : SDL::Renderer, type : Type? = nil)
      SDL::RWops.open(path, "rb") do |rwops|
       if type
          texture = LibIMG.load_texture_typed_rw(renderer, rwops, 1, type.to_s)
          raise Error.new("IMG_LoadTextureTyped_RW") unless texture
        else
          texture = LibIMG.load_texture_rw(renderer, rwops, 1)
          raise Error.new("IMG_LoadTexture_RW") unless texture
        end
        SDL::Texture.new(texture)
      end
    end

    {% for type in Type.constants %}
      # Loads the {{type.id}} image from *path* as a `SDL::Surface`.
      def self.load_{{type.downcase.id}}(path)
        SDL::RWops.open(path, "rb") do |rwops|
          surface = LibIMG.load_{{type.downcase.id}}_rw(rwops, 1)
          raise Error.new("IMG_Load{{type.id}}_RW") unless surface
          SDL::Surface.new(surface)
        end
      end

      # Returns true if the image at *path* is a {{type.id}} file.
      def self.{{type.downcase.id}}?(path)
        SDL::RWops.open(path, "rb") do |rwops|
          LibIMG.is_{{type.downcase.id}}(rwops) == 1
        end
      end
    {% end %}

    #class File
    #  def initialize(path)
    #    @rwops = SDL::RWops.new(path, "rb")
    #  end

    #  # Loads the file as a `SDL::Surface`.
    #  def load
    #    surface = LibIMG.load_rw(@rwops, 1)
    #    raise Error.new("IMG_Load_RW") unless surface
    #    SDL::Surface.new(surface)
    #  end

    #  # Loads the file as a `SDL::Texture` for *renderer*.
    #  def load(renderer : SDL::Renderer)
    #    texture = LibIMG.load_texture_rw(renderer, @rwops, 1)
    #    raise Error.new("IMG_LoadTexture_RW") unless texture
    #    SDL::Texture.new(texture)
    #  end

    #  {% for type in Type.constants %}
    #    # Returns true if the file is a {{type.id}} file.
    #    def {{type.downcase.id}}?
    #      LibIMG.is_{{type.downcase.id}}(@rwops) == 1
    #    end
    #  {% end %}
    #end
  end
end
