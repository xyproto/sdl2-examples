with Ada.Text_IO;
with Ada.Command_Line;

with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces.Makers;
with SDL.Video.Textures.Makers;

with SDL.Images.IO;
with SDL.Events.Events;
with SDL.Error;

procedure Main is

   Window_Title : constant String := "Hello World!";
   Image_Name   : constant String := "../img/grumpy-cat.png";

   use SDL.Video;
   use type Windows.Window_Flags;
   use type Renderers.Renderer_flags;
   use type SDL.Events.Event_Types;

   Win : Windows.Window;
   Ren : Renderers.Renderer;
   Bmp : Surfaces.Surface;
   Tex : Textures.Texture;

   Event : SDL.Events.Events.Events;
begin
   --  Initialise SDL
   if not SDL.Initialise or not SDL.Images.Initialise then
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "SDL.Initialise error: " & SDL.Error.Get);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   --  Create window
   Windows.Makers.Create (Win,
                          Title    => Window_Title,
                          Position => (100, 100),
                          Size     => (620, 387),
                          Flags    => Windows.Windowed or Windows.Shown);

   --  Create renderer
   Renderers.Makers.Create (Rend   => Ren,
                            Window => Win,
                            Flags  => Renderers.Accelerated or Renderers.Present_V_Sync);

   --  Create image
   SDL.Images.IO.Create (Surface   => Bmp,
                         File_Name => Image_Name);

   --  Create texture
   Textures.Makers.Create (Tex      => Tex,
                           Renderer => Ren,
                           Surface  => Bmp);

   --  Present texture
   Ren.Clear;
   Ren.Copy (Copy_From => Tex);
   Ren.Present;

   --  Event loop
   loop
      SDL.Events.Events.Wait (Event);
      exit when Event.Common.Event_Type = SDL.Events.Quit;
      delay 0.010;
   end loop;

   --  Cleanup
   --  Not needed really when soon out of scope
   Tex.Finalize;
   Ren.Finalize;
   Win.Finalize;

   SDL.Finalise;

end Main;
