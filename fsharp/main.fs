module Main

open System
open System.Runtime.InteropServices
open System.Text
open Microsoft.FSharp.NativeInterop

let SDL_INIT_VIDEO : uint32 = 0x00000020u
let SDL_WINDOWPOS_UNDEFINED : int32 = 0x1FFF0000
let SDL_WINDOW_SHOWN : uint32 = 0x00000004u
let SDL_RENDERER_ACCELERATED : uint32 = 0x00000002u
let SDL_RENDERER_PRESENTVSYNC : uint32 = 0x00000004u

[<DllImport("SDL2")>]
extern int32 SDL_Init(uint32 flags)

[<DllImport("SDL2")>]
extern IntPtr SDL_CreateWindow(string title, int x, int y, int w, int h, uint32 flags)

[<DllImport("SDL2")>]
extern IntPtr SDL_CreateRenderer(IntPtr win, int index, uint32 flags)

[<DllImport("SDL2")>]
extern IntPtr SDL_CreateTextureFromSurface(IntPtr renderer, IntPtr surface)

[<DllImport("SDL2")>]
extern void SDL_DestroyRenderer(IntPtr renderer)

[<DllImport("SDL2")>]
extern void SDL_DestroyWindow(IntPtr window)

[<DllImport("SDL2")>]
extern void SDL_FreeSurface(IntPtr surface)

[<DllImport("SDL2")>]
extern void SDL_DestroyTexture(IntPtr texture)

[<DllImport("SDL2")>]
extern int32 SDL_RenderClear(IntPtr renderer)

[<DllImport("SDL2")>]
extern int32 SDL_RenderCopy(IntPtr renderer, IntPtr texture, IntPtr srcrect, IntPtr dstrect)

[<DllImport("SDL2")>]
extern void SDL_RenderPresent(IntPtr renderer)

[<DllImport("SDL2")>]
extern IntPtr SDL_LoadBMP_RW(IntPtr src, int freesrc)

[<DllImport("SDL2")>]
extern IntPtr SDL_RWFromFile(string filename, string permissions)

[<DllImport("SDL2")>]
extern void SDL_Quit()

[<DllImport("SDL2")>]
extern IntPtr SDL_GetError()

let printErr (topic: string) =
    let errPtr = SDL_GetError()
    let errString = Marshal.PtrToStringAnsi(errPtr)
    Console.Error.WriteLine($"{topic} Error: {errString}")

[<EntryPoint>]
let main argv =
    if SDL_Init(SDL_INIT_VIDEO) <> 0 then
        printErr "SDL_Init"
        1
    else
        let win = SDL_CreateWindow("Hello, World!", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 620, 387, SDL_WINDOW_SHOWN)
        if win = IntPtr.Zero then
            printErr "SDL_CreateWindow"
            1
        else
            let ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED ||| SDL_RENDERER_PRESENTVSYNC)
            if ren = IntPtr.Zero then
                printErr "SDL_CreateRenderer"
                SDL_DestroyWindow win
                SDL_Quit()
                1
            else
                let rwop = SDL_RWFromFile("../img/grumpy-cat.bmp", "rb")
                if rwop = IntPtr.Zero then
                    printErr "SDL_RWFromFile"
                    SDL_DestroyRenderer ren
                    SDL_DestroyWindow win
                    SDL_Quit()
                    1
                else
                    let bmp = SDL_LoadBMP_RW(rwop, 1) // this also frees rwop
                    if bmp = IntPtr.Zero then
                        printErr "SDL_LoadBMP_RW"
                        SDL_DestroyRenderer ren
                        SDL_DestroyWindow win
                        SDL_Quit()
                        1
                    else
                        let tex = SDL_CreateTextureFromSurface(ren, bmp)
                        SDL_FreeSurface(bmp)
                        if tex = IntPtr.Zero then
                            printErr "SDL_CreateTextureFromSurface"
                            SDL_DestroyRenderer ren
                            SDL_DestroyWindow win
                            SDL_Quit()
                            1
                        else
                            for i in 1 .. 20 do
                                SDL_RenderClear ren |> ignore
                                SDL_RenderCopy(ren, tex, IntPtr.Zero, IntPtr.Zero) |> ignore
                                SDL_RenderPresent ren
                                System.Threading.Thread.Sleep(100)
                            SDL_DestroyTexture tex
                            SDL_DestroyRenderer ren
                            SDL_DestroyWindow win
                            SDL_Quit()
                            0
