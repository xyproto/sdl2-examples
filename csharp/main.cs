using System;
using System.Runtime.InteropServices;
using System.Text;

public class HelloWorld
{
    // From SDL.h
    private const UInt32 SDL_INIT_VIDEO = 0x00000020;

    // From SDL_video.h
    private const Int32 SDL_WINDOWPOS_UNDEFINED = 0x1FFF0000;
    private const UInt32 SDL_WINDOW_SHOWN = 0x00000004;

    // From SDL_render.h
    private const UInt32 SDL_RENDERER_ACCELERATED = 0x00000002;
    private const UInt32 SDL_RENDERER_PRESENTVSYNC = 0x00000004;

    [DllImport("SDL2")]
    private static extern unsafe Int32 SDL_Init(UInt32 flags);

    [DllImport("SDL2")]
    private static extern unsafe IntPtr SDL_CreateWindow(
        [MarshalAs(UnmanagedType.LPStr)] string title,
        int x,
        int y,
        int w,
        int h,
        uint flags
    );

    [DllImport("SDL2")]
    private static extern unsafe IntPtr SDL_CreateRenderer(
        IntPtr win,
        int index,
        uint flags
    );

    [DllImport("SDL2")]
    private static extern unsafe IntPtr SDL_CreateTextureFromSurface(
        IntPtr renderer,
        IntPtr surface
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_DestroyRenderer(
        IntPtr renderer
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_DestroyWindow(
        IntPtr window
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_FreeSurface(
        IntPtr surface
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_DestroyTexture(
        IntPtr texture
    );

    [DllImport("SDL2")]
    private static extern unsafe Int32 SDL_RenderClear(
        IntPtr renderer
    );

    [DllImport("SDL2")]
    private static extern unsafe Int32 SDL_RenderCopy(
        IntPtr renderer,
        IntPtr texture,
        IntPtr srcrect,
        IntPtr dstrect
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_RenderPresent(
        IntPtr renderer
    );

    [DllImport("SDL2")]
    private static extern unsafe IntPtr SDL_LoadBMP_RW(
        IntPtr src,
        int freesrc
    );

    [DllImport("SDL2")]
    private static extern unsafe IntPtr SDL_RWFromFile(
        [MarshalAs(UnmanagedType.LPStr)] string filename,
        [MarshalAs(UnmanagedType.LPStr)] string permissions
    );

    [DllImport("SDL2")]
    private static extern unsafe void SDL_Quit();

    [DllImport("SDL2")]
    private static extern unsafe byte* SDL_GetError();

    // Call SDL_GetError() and return the C string as a C# String
    private static unsafe string SDL_GetErrorString()
    {
        StringBuilder sb = new StringBuilder();
        byte* errStr = SDL_GetError();
        int i = 0;
        while (errStr[i] != 0) // trust that the string returned from SDL_GetError() is properly terminated
        {
            sb.Append(Convert.ToChar(errStr[i]));
            i++;
        }
        return sb.ToString();
    }

    // Print the SDL_GetError() error message to stderr, with a preceding topic and also " Error: "
    private static void PrintErr(string topic)
    {
        Console.Error.WriteLine(topic + " Error: " + SDL_GetErrorString());
    }

    public class SDLWindow : IDisposable
    {
        public IntPtr Handle { get; private set; }

        public SDLWindow(string title, int x, int y, int w, int h, uint flags)
        {
            Handle = SDL_CreateWindow(title, x, y, w, h, flags);
            if (Handle == IntPtr.Zero)
            {
                throw new Exception("SDL_CreateWindow Error: " + SDL_GetErrorString());
            }
        }

        public void Dispose()
        {
            if (Handle != IntPtr.Zero)
            {
                SDL_DestroyWindow(Handle);
                Handle = IntPtr.Zero;
            }
        }
    }

    public class SDLRenderer : IDisposable
    {
        public IntPtr Handle { get; private set; }

        public SDLRenderer(SDLWindow window, int index, uint flags)
        {
            Handle = SDL_CreateRenderer(window.Handle, index, flags);
            if (Handle == IntPtr.Zero)
            {
                throw new Exception("SDL_CreateRenderer Error: " + SDL_GetErrorString());
            }
        }

        public void Dispose()
        {
            if (Handle != IntPtr.Zero)
            {
                SDL_DestroyRenderer(Handle);
                Handle = IntPtr.Zero;
            }
        }
    }

    public class SDLTexture : IDisposable
    {
        public IntPtr Handle { get; private set; }

        public SDLTexture(SDLRenderer renderer, IntPtr surface)
        {
            Handle = SDL_CreateTextureFromSurface(renderer.Handle, surface);
            if (Handle == IntPtr.Zero)
            {
                throw new Exception("SDL_CreateTextureFromSurface Error: " + SDL_GetErrorString());
            }
        }

        public void Dispose()
        {
            if (Handle != IntPtr.Zero)
            {
                SDL_DestroyTexture(Handle);
                Handle = IntPtr.Zero;
            }
        }
    }

    public static int Main(string[] args)
    {
        if (SDL_Init(SDL_INIT_VIDEO) != 0)
        {
            PrintErr("SDL_Init");
            return 1;
        }

        using (var window = new SDLWindow("Hello, World!", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 620, 387, SDL_WINDOW_SHOWN))
        using (var renderer = new SDLRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC))
        {
            IntPtr rwop = SDL_RWFromFile("../img/grumpy-cat.bmp", "rb");
            if (rwop == IntPtr.Zero)
            {
                PrintErr("SDL_RWFromFile");
                return 1;
            }

            IntPtr bmp = SDL_LoadBMP_RW(rwop, 1); // this also frees rwop
            if (bmp == IntPtr.Zero)
            {
                PrintErr("SDL_LoadBMP_RW");
                return 1;
            }

            using (var texture = new SDLTexture(renderer, bmp))
            {
                SDL_FreeSurface(bmp);

                for (int i = 0; i < 20; i++)
                {
                    SDL_RenderClear(renderer.Handle);
                    SDL_RenderCopy(renderer.Handle, texture.Handle, IntPtr.Zero, IntPtr.Zero);
                    SDL_RenderPresent(renderer.Handle);
                    System.Threading.Thread.Sleep(100);
                }
            }
        }

        SDL_Quit();

        return 0;
    }
}
