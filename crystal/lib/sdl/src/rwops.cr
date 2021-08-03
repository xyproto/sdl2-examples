module SDL
  class RWops

    @@rw_file = LibSDL.alloc_rw

    @@rw_file.value.size = ->(context : LibSDL::RWops*) {
      file = Box(RWops).unbox(context.value.hidden.unknown.data1).file
      File.size(file.path).to_i64
    }

    @@rw_file.value.seek = ->(context : LibSDL::RWops*, offset : Int64, whence : LibC::Int) {
      wh = case whence
           when LibSDL::RW_SEEK_SET then IO::Seek::Set
           when LibSDL::RW_SEEK_CUR then IO::Seek::Current
           when LibSDL::RW_SEEK_END then IO::Seek::End
           else raise "can't seek: invalid whence value"
           end
      file = Box(RWops).unbox(context.value.hidden.unknown.data1).file
      file.seek(offset.to_i32, wh).tell.to_i64
    }

    @@rw_file.value.read = ->(context : LibSDL::RWops*, ptr : Void*, size : LibC::SizeT, maxnum : LibC::SizeT) {
      file = Box(RWops).unbox(context.value.hidden.unknown.data1).file
      slice = ptr.as(UInt8*).to_slice(maxnum * size)

      begin
        # FIXME: may have read incomplete objects (seek back)!
        count = 0
        while slice.size > 0
          count += read_bytes = file.read(slice)
          break if read_bytes == 0
          slice += read_bytes
        end
        LibC::SizeT.new(count / size)
      rescue ex
        LibC::SizeT.new(0)
      end
    }

    @@rw_file.value.write = ->(context : LibSDL::RWops*, ptr : Void*, size : LibC::SizeT, num : LibC::SizeT) {
      file = Box(RWops).unbox(context.value.hidden.unknown.data1).file
      slice = ptr.as(UInt8*).to_slice(num * size)

      begin
        file.write(slice)
        num
      rescue ex
        LibC::SizeT.new(0)
      end
    }

    @@rw_file.value.close = ->(context : LibSDL::RWops*) {
      Box(RWops).unbox(context.value.hidden.unknown.data1).close
      0
    }

    def self.open(path : String, mode = "rb")
      rwops = new(path, mode)
      begin
        yield rwops
      ensure
        rwops.close
      end
    end

    @file : File
    @context : LibSDL::RWops*
    @freed = false

    protected def initialize(path : String, mode = "rb")
      @file = File.open(path, mode)
      @context = LibSDL.alloc_rw
      @context.value.size = @@rw_file.value.size
      @context.value.seek = @@rw_file.value.seek
      @context.value.read = @@rw_file.value.read
      @context.value.write = @@rw_file.value.write
      @context.value.close = @@rw_file.value.close
      @context.value.type = LibSDL::RWOPS_UNKNOWN
      @context.value.hidden.unknown.data1 = Box.box(self)
    end

    # :nodoc:
    def file
      @file
    end

    def finalize
      close
    end

    def close
      if file = @file
        file.close unless file.closed?
      end
      unless @freed
        @freed = true
        LibSDL.free_rw(@context)
      end
    end

    def to_unsafe
      @context
    end
  end
end
