using System;
using System.Runtime.InteropServices;
using System.Text;
using System.Diagnostics;

public class HelloWorld
{
    // From SDL.h
    private const UInt32 SDL_INIT_TIMER = 0x00000001;
    private const UInt32 SDL_INIT_VIDEO = 0x00000020;
    private const UInt32 SDL_INIT_EVENTS = 0x00004000;

    // From SDL_video.h
    private const Int32 SDL_WINDOWPOS_UNDEFINED = 0x1FFF0000;
    private const UInt32 SDL_WINDOW_SHOWN = 0x00000004;

    // From SDL_render.h
    private const UInt32 SDL_RENDERER_ACCELERATED = 0x00000002;
    private const UInt32 SDL_RENDERER_PRESENTVSYNC = 0x00000004;

    // From SDL_events.h
    private const UInt32 SDL_QUIT = 0x100;
    private const UInt32 SDL_KEYDOWN = 0x300;

    // From SDL_keycode.h
    private const UInt32 SDLK_ESCAPE = 27;

    [StructLayout(LayoutKind.Sequential)]
    public struct SDL_Event
    {
        public UInt32 type;
        public UInt32 timestamp;
        public UInt32 windowID;
        public UInt32 eventState;
        public SDL_Keysym key;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct SDL_Keysym
    {
        public UInt32 scancode;
        public UInt32 sym;
        public UInt32 mod;
        public UInt32 unused;
    }

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

    [DllImport("SDL2")]
    private static extern unsafe int SDL_PollEvent(out SDL_Event e);

    [DllImport("SDL2")]
    private static extern unsafe UInt32 SDL_GetTicks();

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
        public IntPtr Handle {
            get;
            private set;
        }

        public SDLWindow(string title, int x, int y, int w, int h, uint flags)
        {
            Handle = SDL_CreateWindow(title, x, y, w, h, flags);
            if (Handle == IntPtr.Zero)
            {
                throw new Exception("SDL_CreateWindow Error: " + SDL_GetErrorString());
            }
            Console.WriteLine("SDLWindow created successfully");
        }

        public void Dispose()
        {
            if (Handle != IntPtr.Zero)
            {
                SDL_DestroyWindow(Handle);
                Handle = IntPtr.Zero;
                Console.WriteLine("SDLWindow destroyed");
            }
        }
    }

    public class SDLRenderer : IDisposable
    {
        public IntPtr Handle {
            get;
            private set;
        }

        public SDLRenderer(SDLWindow window, int index, uint flags)
        {
            if (window.Handle == IntPtr.Zero)
            {
                throw new Exception("Invalid window handle when creating renderer.");
            }

            Handle = SDL_CreateRenderer(window.Handle, index, flags);
            if (Handle == IntPtr.Zero)
            {
                throw new Exception("SDL_CreateRenderer Error: " + SDL_GetErrorString());
            }
            Console.WriteLine("SDLRenderer created successfully");
        }

        public void Dispose()
        {
            if (Handle != IntPtr.Zero)
            {
                SDL_DestroyRenderer(Handle);
                Handle = IntPtr.Zero;
                Console.WriteLine("SDLRenderer destroyed");
            }
        }
    }

    public class SDLTexture : IDisposable
    {
        public IntPtr Handle {
            get;
            private set;
        }

        public SDLTexture(SDLRenderer renderer, IntPtr surface)
        {
            if (renderer.Handle == IntPtr.Zero)
            {
                throw new Exception("Renderer handle is null when creating texture.");
            }

            Handle = SDL_CreateTextureFromSurface(renderer.Handle, surface);
            if (Handle == IntPtr.Zero)
            {
                throw new Exception("SDL_CreateTextureFromSurface Error: " + SDL_GetErrorString());
            }
            Console.WriteLine("SDLTexture created successfully");
        }

        public void Dispose()
        {
            if (Handle != IntPtr.Zero)
            {
                SDL_DestroyTexture(Handle);
                Handle = IntPtr.Zero;
                Console.WriteLine("SDLTexture destroyed");
            }
        }
    }

    public static int Main(string[] args)
    {
        if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_EVENTS) != 0)
        {
            PrintErr("SDL_Init");
            return 1;
        }
        Console.WriteLine("SDL Initialized successfully");

        using (var window = new SDLWindow("Hello, World!", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 620, 387, SDL_WINDOW_SHOWN))
            using (var renderer = new SDLRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC))
            {
                IntPtr rwop = SDL_RWFromFile("../img/grumpy-cat.bmp", "rb");
                if (rwop == IntPtr.Zero)
                {
                    PrintErr("SDL_RWFromFile");
                    return 1;
                }
                Console.WriteLine("Image file loaded successfully");

                IntPtr bmp = SDL_LoadBMP_RW(rwop, 1); // this also frees rwop
                if (bmp == IntPtr.Zero)
                {
                    PrintErr("SDL_LoadBMP_RW");
                    return 1;
                }
                Console.WriteLine("BMP loaded successfully");

                using (var texture = new SDLTexture(renderer, bmp))
                {
                    SDL_FreeSurface(bmp);
                    Console.WriteLine("Surface freed successfully");

                    Debug.Assert(renderer.Handle != IntPtr.Zero, "Renderer handle is null!");
                    Debug.Assert(texture.Handle != IntPtr.Zero, "Texture handle is null!");

                    if (renderer.Handle == IntPtr.Zero || texture.Handle == IntPtr.Zero)
                    {
                        PrintErr("Null reference in renderer or texture handle");
                        return 1;
                    }

                    SDL_Event e;
                    bool quit = false;
                    UInt32 startTime = SDL_GetTicks();

                    while (!quit)
                    {
                        // Poll for events
                        while (SDL_PollEvent(out e) != 0)
                        {
                            if (e.type == SDL_QUIT || (e.type == SDL_KEYDOWN && e.key.sym == SDLK_ESCAPE))
                            {
                                quit = true;
                            }
                        }

                        // Render loop
                        if (renderer.Handle != IntPtr.Zero && texture.Handle != IntPtr.Zero)
                        {
                            Console.WriteLine("Rendering frame");
                            SDL_RenderClear(renderer.Handle);
                            SDL_RenderCopy(renderer.Handle, texture.Handle, IntPtr.Zero, IntPtr.Zero);
                            SDL_RenderPresent(renderer.Handle);
                        }
                        else
                        {
                            PrintErr("Renderer or texture handle became null during render loop");
                            break;
                        }

                        // Exit after 2000ms (2 seconds)
                        if (SDL_GetTicks() - startTime > 2000)
                        {
                            break;
                        }

                        System.Threading.Thread.Sleep(100); // Delay to limit loop iteration
                    }
                }
            }

        SDL_Quit();
        Console.WriteLine("SDL Quit successfully");

        return 0;
    }
}
