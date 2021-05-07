using System;
using System.Runtime.InteropServices;
using System.Text;

public class HelloWorld
{
    private const Int32 windowpos_undefined    = 0x1FFF0000;
    private const UInt32 init_video            = 0x00000020;
    private const UInt32 window_shown          = 0x00000004;
    private const UInt32 renderer_accelerated  = 0x00000002;
    private const UInt32 renderer_presentvsync = 0x00000004;

    [DllImport("SDL2")]
    private static extern unsafe Int32 SDL_Init(UInt32 flags);

    [DllImport("SDL2")]
    private static extern unsafe IntPtr SDL_CreateWindow(
        [MarshalAs(UnmanagedType.LPStr)]String title,
        [MarshalAs(UnmanagedType.I4)]Int32 x,
        [MarshalAs(UnmanagedType.I4)]Int32 y,
        [MarshalAs(UnmanagedType.I4)]Int32 w,
        [MarshalAs(UnmanagedType.I4)]Int32 h,
        [MarshalAs(UnmanagedType.U4)]UInt32 flags
    );

    [DllImport("SDL2")]
    private static extern unsafe IntPtr SDL_CreateRenderer(
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr win,
        [MarshalAs(UnmanagedType.I4)]Int32 index,
        [MarshalAs(UnmanagedType.U4)]UInt32 flags
    );

    [DllImport("SDL2")]
    private static extern unsafe IntPtr SDL_CreateTextureFromSurface(
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr renderer,
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr surface
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_DestroyRenderer(
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr renderer
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_DestroyWindow(
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr window
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_FreeSurface(
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr surface
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_DestroyTexture(
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr texture
    );

    [DllImport("SDL2")]
    private static extern unsafe Int32 SDL_RenderClear(
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr renderer
    );

    [DllImport("SDL2")]
    private static extern unsafe Int32 SDL_RenderCopy(
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr renderer,
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr texture,
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr srcrect,
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr dstrect
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_RenderPresent(
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr renderer
    );

    [DllImport("SDL2")]
    private static extern unsafe IntPtr SDL_LoadBMP_RW(
        [MarshalAs(UnmanagedType.LPStruct)]IntPtr src,
        [MarshalAs(UnmanagedType.I4)]Int32 freesrc
    );

    [DllImport("SDL2")]
    private static extern unsafe IntPtr SDL_RWFromFile(
        [MarshalAs(UnmanagedType.LPStr)]String filename,
        [MarshalAs(UnmanagedType.LPStr)]String permissions
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_Quit();

    [DllImport("SDL2")]
    private static extern unsafe byte* SDL_GetError();

    // Call SDL_GetError() and return the C string as a C# String
    private static unsafe String getErr() {
        StringBuilder sb = new StringBuilder();
        byte* errStr = SDL_GetError();
        int i = 0;
        while (errStr[i] != 0) { // trust that the string returned from SDL_GetError() is properly terminated
            sb.Append(Convert.ToChar(errStr[i]));
            i++;
        }
        return sb.ToString();
    }

    // Print the SDL_GetError() error message to stderr, with a preceding topic and also " Error: "
    private static void printErr(String topic) {
        Console.Error.WriteLine(topic + " Error: " + getErr());
    }

    public static int Main(string[] args)
    {
        if (SDL_Init(init_video) != 0) {
            printErr("SDL_Init");
            return 1;
        }

        IntPtr win = SDL_CreateWindow("Hello, World!", windowpos_undefined, windowpos_undefined, 620, 387, window_shown);
        if (win == IntPtr.Zero) {
            printErr("SDL_CreateWindow");
            return 1;
        }

        IntPtr ren = SDL_CreateRenderer(win, -1, renderer_accelerated | renderer_presentvsync);
        if (ren == IntPtr.Zero) {
            printErr("SDL_CreateRenderer");
            SDL_DestroyWindow(win);
            SDL_Quit();
            return 1;
        }

        IntPtr rwop = SDL_RWFromFile("../img/grumpy-cat.bmp", "rb");
        if (rwop == IntPtr.Zero) {
            printErr("SDL_RWFromFile");
            SDL_DestroyRenderer(ren);
            SDL_DestroyWindow(win);
            SDL_Quit();
            return 1;
        }

        IntPtr bmp = SDL_LoadBMP_RW(rwop, 1); // this also frees rwop
        if (bmp == IntPtr.Zero) {
            printErr("SDL_LoadBMP_RW");
            SDL_DestroyRenderer(ren);
            SDL_DestroyWindow(win);
            SDL_Quit();
            return 1;
        }

        IntPtr tex = SDL_CreateTextureFromSurface(ren, bmp);
        if (tex == IntPtr.Zero) {
            printErr("SDL_CreateTextureFromSurface");
            SDL_FreeSurface(bmp);
            SDL_DestroyRenderer(ren);
            SDL_DestroyWindow(win);
            SDL_Quit();
            return 1;
        }
        SDL_FreeSurface(bmp);

        for (int i = 0; i < 20; i++) {
            SDL_RenderClear(ren);
            SDL_RenderCopy(ren, tex, IntPtr.Zero, IntPtr.Zero);
            SDL_RenderPresent(ren);
            System.Threading.Thread.Sleep(100);
        }

        SDL_DestroyTexture(tex);
        SDL_DestroyRenderer(ren);
        SDL_DestroyWindow(win);
        SDL_Quit();

        return 0;
    }
}
