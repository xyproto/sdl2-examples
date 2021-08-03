require "./pixels"

lib LibSDL
  alias Point = SDL::Point
  alias Rect = SDL::Rect

  fun has_intersection = SDL_HasIntersection(a : Rect*, b : Rect*) : Bool
  fun intersect_rect = SDL_IntersectRect(a : Rect*, b : Rect*, result : Rect*) : Bool
  fun union_rect = SDL_UnionRect(a : Rect*, b : Rect*, result : Rect*)
  fun enclose_points = SDL_EnclosePoints(points : Point*, count : Int, clip : Rect*, result : Rect*) : Bool
  fun intersect_rect_and_line = SDL_IntersectRectAndLine(rect : Rect*, x1 : Int*, y1 : Int*, x2 : Int*, y2 : Int*) : Bool
end
