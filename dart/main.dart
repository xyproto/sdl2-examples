import 'dart:ffi';
import 'package:ffi/ffi.dart';

// From SDL.h
const int SDL_INIT_VIDEO = 0x00000020;

// From SDL_video.h
const int SDL_WINDOWPOS_UNDEFINED = 0x1FFF0000;
const int SDL_WINDOW_SHOWN = 0x00000004;

// From SDL_render.h
const int SDL_RENDERER_ACCELERATED = 0x00000002;
const int SDL_RENDERER_PRESENTVSYNC = 0x00000004;

// Load the SDL2 library
final DynamicLibrary sdlLib = DynamicLibrary.open('libSDL2.so');

// Define the signature of the C functions
typedef SDL_InitC = Int32 Function(Uint32 flags);
typedef SDL_InitDart = int Function(int flags);

typedef SDL_CreateWindowC = Pointer Function(Pointer<Utf8> title, Int32 x, Int32 y, Int32 w, Int32 h, Uint32 flags);
typedef SDL_CreateWindowDart = Pointer Function(Pointer<Utf8> title, int x, int y, int w, int h, int flags);

typedef SDL_CreateRendererC = Pointer Function(Pointer win, Int32 index, Uint32 flags);
typedef SDL_CreateRendererDart = Pointer Function(Pointer win, int index, int flags);

typedef SDL_CreateTextureFromSurfaceC = Pointer Function(Pointer renderer, Pointer surface);
typedef SDL_CreateTextureFromSurfaceDart = Pointer Function(Pointer renderer, Pointer surface);

typedef SDL_DestroyRendererC = Void Function(Pointer renderer);
typedef SDL_DestroyRendererDart = void Function(Pointer renderer);

typedef SDL_DestroyWindowC = Void Function(Pointer window);
typedef SDL_DestroyWindowDart = void Function(Pointer window);

typedef SDL_FreeSurfaceC = Void Function(Pointer surface);
typedef SDL_FreeSurfaceDart = void Function(Pointer surface);

typedef SDL_DestroyTextureC = Void Function(Pointer texture);
typedef SDL_DestroyTextureDart = void Function(Pointer texture);

typedef SDL_RenderClearC = Int32 Function(Pointer renderer);
typedef SDL_RenderClearDart = int Function(Pointer renderer);

typedef SDL_RenderCopyC = Int32 Function(Pointer renderer, Pointer texture, Pointer srcrect, Pointer dstrect);
typedef SDL_RenderCopyDart = int Function(Pointer renderer, Pointer texture, Pointer srcrect, Pointer dstrect);

typedef SDL_RenderPresentC = Void Function(Pointer renderer);
typedef SDL_RenderPresentDart = void Function(Pointer renderer);

typedef SDL_RWFromFileC = Pointer Function(Pointer<Utf8> file, Pointer<Utf8> mode);
typedef SDL_RWFromFileDart = Pointer Function(Pointer<Utf8> file, Pointer<Utf8> mode);

typedef SDL_LoadBMP_RWC = Pointer Function(Pointer src, Int32 freesrc);
typedef SDL_LoadBMP_RWDart = Pointer Function(Pointer src, int freesrc);

typedef SDL_DelayC = Void Function(Uint32 ms);
typedef SDL_DelayDart = void Function(int ms);

typedef SDL_QuitC = Void Function();
typedef SDL_QuitDart = void Function();

void main() {
  // Look up the functions
  final SDL_Init = sdlLib.lookupFunction<SDL_InitC, SDL_InitDart>('SDL_Init');
  final SDL_CreateWindow = sdlLib.lookupFunction<SDL_CreateWindowC, SDL_CreateWindowDart>('SDL_CreateWindow');
  final SDL_CreateRenderer = sdlLib.lookupFunction<SDL_CreateRendererC, SDL_CreateRendererDart>('SDL_CreateRenderer');
  final SDL_CreateTextureFromSurface = sdlLib.lookupFunction<SDL_CreateTextureFromSurfaceC, SDL_CreateTextureFromSurfaceDart>('SDL_CreateTextureFromSurface');
  final SDL_DestroyRenderer = sdlLib.lookupFunction<SDL_DestroyRendererC, SDL_DestroyRendererDart>('SDL_DestroyRenderer');
  final SDL_DestroyWindow = sdlLib.lookupFunction<SDL_DestroyWindowC, SDL_DestroyWindowDart>('SDL_DestroyWindow');
  final SDL_FreeSurface = sdlLib.lookupFunction<SDL_FreeSurfaceC, SDL_FreeSurfaceDart>('SDL_FreeSurface');
  final SDL_DestroyTexture = sdlLib.lookupFunction<SDL_DestroyTextureC, SDL_DestroyTextureDart>('SDL_DestroyTexture');
  final SDL_RenderClear = sdlLib.lookupFunction<SDL_RenderClearC, SDL_RenderClearDart>('SDL_RenderClear');
  final SDL_RenderCopy = sdlLib.lookupFunction<SDL_RenderCopyC, SDL_RenderCopyDart>('SDL_RenderCopy');
  final SDL_RenderPresent = sdlLib.lookupFunction<SDL_RenderPresentC, SDL_RenderPresentDart>('SDL_RenderPresent');
  final SDL_RWFromFile = sdlLib.lookupFunction<SDL_RWFromFileC, SDL_RWFromFileDart>('SDL_RWFromFile');
  final SDL_LoadBMP_RW = sdlLib.lookupFunction<SDL_LoadBMP_RWC, SDL_LoadBMP_RWDart>('SDL_LoadBMP_RW');
  final SDL_Delay = sdlLib.lookupFunction<SDL_DelayC, SDL_DelayDart>('SDL_Delay');
  final SDL_Quit = sdlLib.lookupFunction<SDL_QuitC, SDL_QuitDart>('SDL_Quit');

  SDL_Init(SDL_INIT_VIDEO);

  final titleCString = 'Hello, World!'.toNativeUtf8();
  var window = SDL_CreateWindow(titleCString, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 620, 387, SDL_WINDOW_SHOWN);

  var renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);

  final bmpFileCString = '../img/grumpy-cat.bmp'.toNativeUtf8();
  final modeCString = 'rb'.toNativeUtf8();
  var rwop = SDL_RWFromFile(bmpFileCString, modeCString);

  var bmp = SDL_LoadBMP_RW(rwop, 1);
  var texture = SDL_CreateTextureFromSurface(renderer, bmp);
  SDL_FreeSurface(bmp);

  for (int i = 0; i < 20; i++) {
    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, texture, nullptr, nullptr);
    SDL_RenderPresent(renderer);
    SDL_Delay(100);
  }

  SDL_DestroyTexture(texture);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  calloc.free(titleCString);
  calloc.free(bmpFileCString);
  calloc.free(modeCString);

  SDL_Quit();
}
