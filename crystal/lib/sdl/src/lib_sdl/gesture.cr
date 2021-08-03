lib LibSDL
  alias GestureID = Int64

  fun record_gesture = RecordGesture(touchId : TouchID) : Int
  fun save_all_dollar_templates = SaveAllDollarTemplates(dst : RWops*) : Int
  fun save_dollar_template = SaveDollarTemplate(gestureId : GestureID, dst : RWops*) : Int
  fun load_dollar_templates = LoadDollarTemplates(touchId : TouchID, src : RWops*) : Int
end
