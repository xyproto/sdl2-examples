open Tsdl

let run () =
  let (let*) = Result.bind in
  let* _ = Sdl.init Sdl.Init.everything in
  let* (w, r) = Sdl.create_window_and_renderer ~w:620 ~h:387 Sdl.Window.shown in
  let* bmp = Sdl.load_bmp "../img/grumpy-cat.bmp" in
  let* txt = Sdl.create_texture_from_surface r bmp in
  Sdl.free_surface bmp;

  for i = 0 to 20 do
    ignore @@ Sdl.render_clear r;
    ignore @@ Sdl.render_copy r txt;
    Sdl.render_present r;
    Sdl.delay (Int32.of_int 100);
  done;

  Sdl.destroy_texture txt;
  Sdl.destroy_renderer r;
  Sdl.destroy_window w;
  Sdl.quit ();
  Ok ()

let () =
  match run () with
  | Ok _ -> exit 0
  | Error (`Msg e) ->
     Sdl.log "Error: %s" e;
     exit 1
