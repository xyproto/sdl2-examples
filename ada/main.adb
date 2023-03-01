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
   Dummy : Boolean;

begin

   --  Initialise SDL
   if not SDL.Initialise or not SDL.Images.Initialise then
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "SDL.Initialize error: " & SDL.Error.Get);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   --  Create window
   Windows.Makers.Create (Win,
                          Title    => Window_Title,
                          Position => (100, 100),
                          Size     => (620, 387),
                          Flags    => Windows.Shown);

   --  Create renderer
   Renderers.Makers.Create (Rend   => Ren,
                            Window => Win,
                            Flags  => (Renderers.Accelerated or
                                         Renderers.Present_V_Sync));

   --  Create image
   SDL.Images.IO.Create (Surface   => Bmp,
                         File_Name => Image_Name);

   --  Create texture
   Textures.Makers.Create (Tex      => Tex,
                           Renderer => Ren,
                           Surface  => Bmp);

   --  Event loop
   --  Exit after 2 seconds, but present the image for each loop
   for I in 1 .. 200 loop

      --  Present texture
      Ren.Clear;
      Ren.Copy (Copy_From => Tex);
      Ren.Present;

      --  Check for the Quit event
      Dummy := SDL.Events.Events.Poll (Event);
      exit when Event.Common.Event_Type = SDL.Events.Quit;

      delay 0.01;

   end loop;

   --  Not really needed since these will soon go out of scope
   Tex.Finalize;
   Ren.Finalize;
   Win.Finalize;
   SDL.Finalise;

end Main;
