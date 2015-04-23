open Sdl

let () =
  Sdl.init [`VIDEO];
  let win =
    Window.create2
      ~title:"Hello World!"
      ~x:`undefined ~y:`undefined
      ~width:960 ~height:540
      ~flags:[Window.Shown]
  in
  let ren =
    Render.create_renderer
      ~win:win
      ~index:~-1
      ~flags:[Render.Accelerated; Render.PresentVSync]
  in
  let img = Surface.load_bmp "../img/boxes.bmp" in
  let tex = Texture.create_from_surface ren img in
    Render.clear ren;
  let src_rect = Rect.make4 0 0 960 540 in
  let dst_rect = Rect.make4 0 0 960 540 in
    Render.copy ren ~texture:tex ~src_rect ~dst_rect ();
    Render.render_present ren;
    Timer.delay 2000;
  Sdl.quit ()
