lib LibSDL
  RWOPS_UNKNOWN   = 0 # Unknown stream type
  RWOPS_WINFILE   = 1 # Win32 file
  RWOPS_STDFILE   = 2 # Stdio file
  RWOPS_JNIFILE   = 3 # Android asset
  RWOPS_MEMORY    = 4 # Memory stream
  RWOPS_MEMORY_RO = 5 # Read-Only memory stream

  RW_SEEK_SET = 0 # Seek from the beginning of data
  RW_SEEK_CUR = 1 # Seek relative to current read point
  RW_SEEK_END = 2 # Seek relative to the end of data

  #{% if flag?(:android) %}
  #  struct RWops_androidio
  #    fileNameRef : Void*
  #    inputStreamRef : Void*
  #    readableByteChannelRef : Void*
  #    readMethod : Void*
  #    assetFileDescriptorRef : Void*
  #    position : Long*
  #    offset : Long*
  #    fd : Int
  #  end
  #{% end %}

  #{% if flag?(:windows) %}
  #  struct RWops_windowsio
  #    append : Int
  #    h : Void*
  #    buffer_data : Void*
  #    buffer_size : Int
  #    buffer_left : Int
  #  end
  #{% else %}
  #  struct RWops_stdio
  #    autoclose : Int
  #    fp : Void* # FILE*
  #  end
  #{% end %}

  struct RWops_mem
    base : UInt8*
    here : UInt8*
    stop : UInt8*
  end

  struct RWops_unknown
    data1 : Void*
    data2 : Void*
  end

  union RWops_hidden
    #{% if flag?(:android) %}
    #  androidio : RWops_androidio
    #{% end %}

    #{% if flag?(:windows) %}
    #  windowsio : RWops_windowsio
    #{% else %}
    #  stdio : RWops_stdio
    #{% end %}

    mem : RWops_mem
    unknown : RWops_unknown
  end

  struct RWops
    size : (RWops*) -> Int64                       # (context)
    seek : (RWops*, Int64, Int) -> Int64           # (context, offset, whence)
    read : (RWops*, Void*, SizeT, SizeT) -> SizeT  # (context, ptr, size, maxnum)
    write : (RWops*, Void*, SizeT, SizeT) -> SizeT # (context, ptr, size, num)
    close : (RWops*) -> Int                        # (context)
    type : UInt32
    hidden : RWops_hidden
  end

  fun rw_from_file = SDL_RWFromFile(file : Char*, mode : Char*) : RWops*

  {% if flag?(:windows) %}
    fun rw_from_fp = SDL_RWFromFP(fp : Void*, autoclose : Int) : RWops*
  {% else %}
    #fun rw_from_fp = SDL_RWFromFP(fp : FILE*, autoclose : Int) : RWops*
    fun rw_from_fp = SDL_RWFromFP(fp : Void*, autoclose : Int) : RWops*
  {% end %}

  fun rw_from_mem = SDL_RWFromMem(mem : Void*, size : Int) : RWops*
  fun rw_from_const_mem = SDL_RWFromConstMem(mem : Void*, size : Int) : RWops*
  fun alloc_rw = SDL_AllocRW() : RWops*
  fun free_rw = SDL_FreeRW(area : RWops*)

  # SDL_RWsize(ctx)                 (ctx)->size(ctx)
  # SDL_RWseek(ctx, offset, whence) (ctx)->seek(ctx, offset, whence)
  # SDL_RWtell(ctx)                 (ctx)->seek(ctx, 0, RW_SEEK_CUR)
  # SDL_RWread(ctx, ptr, size, n)   (ctx)->read(ctx, ptr, size, n)
  # SDL_RWwrite(ctx, ptr, size, n)  (ctx)->write(ctx, ptr, size, n)
  # SDL_RWclose(ctx)                (ctx)->close(ctx)

  fun read_le16 = SDL_ReadLE16(src : RWops*);
  fun read_be16 = SDL_ReadBE16(src : RWops*);
  fun read_le32 = SDL_ReadLE32(src : RWops*);
  fun read_be32 = SDL_ReadBE32(src : RWops*);
  fun read_le64 = SDL_ReadLE64(src : RWops*);
  fun read_be64 = SDL_ReadBE64(src : RWops*);

  fun write_le16 = SDL_WriteLE16(dst : RWops*, value : UInt16)
  fun write_be16 = SDL_WriteBE16(dst : RWops*, value : UInt16)
  fun write_le32 = SDL_WriteLE32(dst : RWops*, value : UInt32)
  fun write_be32 = SDL_WriteBE32(dst : RWops*, value : UInt32)
  fun write_le64 = SDL_WriteLE64(dst : RWops*, value : UInt64)
  fun write_be64 = SDL_WriteBE64(dst : RWops*, value : UInt64)
end
