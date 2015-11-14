program sdltest;

uses SDL2;
var
  win : PSDL_Window;
  ren : PSDL_Renderer;
  bmp : PSDL_Surface;
  tex : PSDL_Texture;
  i : Integer;
begin
  if SDL_Init(SDL_INIT_EVERYTHING) <> 0 then begin
    WriteLn('SDL_Init Error: ', SDL_GetError());
    Halt(1);
  end;

  win := SDL_CreateWindow('Hello World', 100, 100, 960, 540, SDL_WINDOW_SHOWN);
  if win = nil then begin
    WriteLn('SDL_CreateWindow Error: ', SDL_GetError());
    Halt(1);
  end;

  ren := SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
  if ren = nil then begin
    WriteLn('SDL_CreateRenderer Error: ', SDL_GetError());
    Halt(1);
  end;

  bmp := SDL_LoadBMP('../img/boxes.bmp');
  if bmp = nil then begin
    WriteLn('SDL_LoadBMP Error: ', SDL_GetError());
    Halt(1);
  end;

  tex := SDL_CreateTextureFromSurface(ren, bmp);
  SDL_FreeSurface(bmp);
  if tex = nil then begin
    WriteLn('SDL_CreateTextureFromSurface Error: ', SDL_GetError());
    Halt(1);
  end;

  for i := 0 to 20 do begin
    SDL_RenderClear(ren);
    SDL_RenderCopy(ren, tex, nil, nil);
    SDL_RenderPresent(ren);
    SDL_Delay(100);
  end;

  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(ren);
  SDL_DestroyWindow(win);

  SDL_Quit();
end.
