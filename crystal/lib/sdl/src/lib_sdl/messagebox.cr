lib LibSDL
  enum MessageBoxFlags : UInt32
    ERROR        = 0x00000010
    WARNING      = 0x00000020
    INFORMATION  = 0x00000040
  end

  enum MessageBoxButtonFlags : UInt32
    RETURNKEY_DEFAULT = 0x00000001
    ESCAPEKEY_DEFAULT = 0x00000002
  end

  struct MessageBoxButtonData
    flags : UInt32
    buttonid : Int
    text : Char*
  end

  struct  MessageBoxColor
    r : UInt8
    g : UInt8
    b : UInt8
  end

  enum MessageBoxColorType
    BACKGROUND
    TEXT
    BUTTON_BORDER
    BUTTON_BACKGROUND
    BUTTON_SELECTED
    MAX
  end

  struct MessageBoxColorScheme
    colors : MessageBoxColor[6] # 6 == MessageBoxColorType::MAX
  end

  struct MessageBoxData
    flags : UInt32
    window : Window*
    title : Char*
    message : Char*
    numbuttons : Int
    buttons : MessageBoxButtonData*
    colorScheme : MessageBoxColorScheme*
  end

  fun show_message_box = SDL_ShowMessageBox(messageboxdata : MessageBoxData*, buttonid : Int*) : Int
  fun show_simple_message_box = SDL_ShowSimpleMessageBox(flags : UInt32, title : Char*, message : Char*, window : Window*) : Int
end
