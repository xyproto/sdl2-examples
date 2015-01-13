open Sdl

let color_of_rgb surf ~rgb =
  let fmt_kind = Surface.get_pixelformat_t surf in
  Printf.printf "# pixel_format kind: %s\n%!"
    (Pixel.get_pixel_format_name fmt_kind);
  let fmt = Pixel.alloc_format fmt_kind in
  let color = Pixel.map_RGB fmt ~rgb in
  Pixel.free_format fmt;
  (color)

let f surf win x y w h ~rgb ~msec =
  let color = color_of_rgb ~rgb surf in
  let rect = Rect.make4 x y w h in
  Surface.fill_rect ~dst:surf ~rect ~color;
  Window.update_surface win;
  Timer.delay msec;
;;

let () =
  Sdl.init [`VIDEO];
  let win =
    Window.create2
      ~title:"OCaml SDL2 rectangles"
      ~x:`centered ~y:`centered
      ~width:640 ~height:480
      ~flags:[Window.Resizable]
  in
  let surf = Window.get_surface win in
  f surf win 20 20 200 120 ~rgb:(0,255,0) ~msec:600;
  f surf win 60 60 200 200 ~rgb:(255,0,0) ~msec:600;
  f surf win  0  0 100 100 ~rgb:(0,0,255) ~msec:2000;
  (*
  Surface.save_bmp surf ~filename:"test.bmp";
  *)
  Sdl.quit ()
