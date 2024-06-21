import static java.lang.foreign.ValueLayout.*;
import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

public class Main {
    private static final Linker linker = Linker.nativeLinker();
    private static SymbolLookup lib;

    // Constants from SDL
    private static final int SDL_INIT_VIDEO = 0x00000020;
    private static final int SDL_WINDOWPOS_UNDEFINED = 0x1FFF0000;
    private static final int SDL_WINDOW_SHOWN = 0x00000004;
    private static final int SDL_RENDERER_ACCELERATED = 0x00000002;
    private static final int SDL_RENDERER_PRESENTVSYNC = 0x00000004;
    private static final int SDL_QUIT = 0x100;
    private static final int SDL_KEYDOWN = 0x300;
    private static final int SDLK_ESCAPE = 0x1B;

    public static void main(String[] args) {
        try {
            runSDL();
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    private static void runSDL() throws Throwable {
        String libPath = System.getProperty("sdl2.library.path");
        if (libPath == null || libPath.isEmpty()) {
            throw new IllegalArgumentException("Library path not specified. Use -Dsdl2.library.path=<path>");
        }

        try (Arena arena = Arena.ofConfined()) {
            lib = SymbolLookup.libraryLookup(libPath, arena);

            MethodHandle SDL_Init = loadFunction("SDL_Init", FunctionDescriptor.of(JAVA_INT, JAVA_INT));
            MethodHandle SDL_Quit = loadFunction("SDL_Quit", FunctionDescriptor.ofVoid());
            MethodHandle SDL_CreateWindow = loadFunction("SDL_CreateWindow", FunctionDescriptor.of(ADDRESS, ADDRESS, JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT));
            MethodHandle SDL_CreateRenderer = loadFunction("SDL_CreateRenderer", FunctionDescriptor.of(ADDRESS, ADDRESS, JAVA_INT, JAVA_INT));
            MethodHandle SDL_CreateTextureFromSurface = loadFunction("SDL_CreateTextureFromSurface", FunctionDescriptor.of(ADDRESS, ADDRESS, ADDRESS));
            MethodHandle SDL_DestroyRenderer = loadFunction("SDL_DestroyRenderer", FunctionDescriptor.ofVoid(ADDRESS));
            MethodHandle SDL_DestroyWindow = loadFunction("SDL_DestroyWindow", FunctionDescriptor.ofVoid(ADDRESS));
            MethodHandle SDL_FreeSurface = loadFunction("SDL_FreeSurface", FunctionDescriptor.ofVoid(ADDRESS));
            MethodHandle SDL_DestroyTexture = loadFunction("SDL_DestroyTexture", FunctionDescriptor.ofVoid(ADDRESS));
            MethodHandle SDL_RenderClear = loadFunction("SDL_RenderClear", FunctionDescriptor.of(JAVA_INT, ADDRESS));
            MethodHandle SDL_RenderCopy = loadFunction("SDL_RenderCopy", FunctionDescriptor.of(JAVA_INT, ADDRESS, ADDRESS, ADDRESS, ADDRESS));
            MethodHandle SDL_RenderPresent = loadFunction("SDL_RenderPresent", FunctionDescriptor.ofVoid(ADDRESS));
            MethodHandle SDL_LoadBMP_RW = loadFunction("SDL_LoadBMP_RW", FunctionDescriptor.of(ADDRESS, ADDRESS, JAVA_INT));
            MethodHandle SDL_RWFromFile = loadFunction("SDL_RWFromFile", FunctionDescriptor.of(ADDRESS, ADDRESS, ADDRESS));
            MethodHandle SDL_GetError = loadFunction("SDL_GetError", FunctionDescriptor.of(ADDRESS));
            MethodHandle SDL_PollEvent = loadFunction("SDL_PollEvent", FunctionDescriptor.of(JAVA_INT, ADDRESS));
            MethodHandle SDL_Delay = loadFunction("SDL_Delay", FunctionDescriptor.ofVoid(JAVA_INT));

            if (SDL_Init == null || SDL_Quit == null || SDL_CreateWindow == null || SDL_CreateRenderer == null || SDL_CreateTextureFromSurface == null ||
                SDL_DestroyRenderer == null || SDL_DestroyWindow == null || SDL_FreeSurface == null || SDL_DestroyTexture == null ||
                SDL_RenderClear == null || SDL_RenderCopy == null || SDL_RenderPresent == null || SDL_LoadBMP_RW == null || SDL_RWFromFile == null || SDL_GetError == null ||
                SDL_PollEvent == null || SDL_Delay == null) {
                System.err.println("Failed to load one or more SDL functions");
                return;
            }

            int result = (int) SDL_Init.invoke(SDL_INIT_VIDEO);
            if (result != 0) {
                System.err.println("SDL_Init failed with error code: " + result);
                return;
            }

            MemorySegment title = allocateString(arena, "Hello, World!");
            MemorySegment window = (MemorySegment) SDL_CreateWindow.invoke(title, 100, 100, 620, 387, SDL_WINDOW_SHOWN);
            if (window.address() == 0) {
                System.err.println("SDL_CreateWindow failed: " + getError(SDL_GetError));
                SDL_Quit.invoke();
                return;
            }

            MemorySegment renderer = (MemorySegment) SDL_CreateRenderer.invoke(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
            if (renderer.address() == 0) {
                System.err.println("SDL_CreateRenderer failed: " + getError(SDL_GetError));
                SDL_DestroyWindow.invoke(window);
                SDL_Quit.invoke();
                return;
            }

            MemorySegment filename = allocateString(arena, "../img/grumpy-cat.bmp");
            MemorySegment rwop = (MemorySegment) SDL_RWFromFile.invoke(filename, allocateString(arena, "rb"));
            if (rwop.address() == 0) {
                System.err.println("SDL_RWFromFile failed: " + getError(SDL_GetError));
                SDL_DestroyRenderer.invoke(renderer);
                SDL_DestroyWindow.invoke(window);
                SDL_Quit.invoke();
                return;
            }

            MemorySegment bmp = (MemorySegment) SDL_LoadBMP_RW.invoke(rwop, 1);
            if (bmp.address() == 0) {
                System.err.println("SDL_LoadBMP_RW failed: " + getError(SDL_GetError));
                SDL_DestroyRenderer.invoke(renderer);
                SDL_DestroyWindow.invoke(window);
                SDL_Quit.invoke();
                return;
            }

            MemorySegment texture = (MemorySegment) SDL_CreateTextureFromSurface.invoke(renderer, bmp);
            SDL_FreeSurface.invoke(bmp);
            if (texture.address() == 0) {
                System.err.println("SDL_CreateTextureFromSurface failed: " + getError(SDL_GetError));
                SDL_DestroyRenderer.invoke(renderer);
                SDL_DestroyWindow.invoke(window);
                SDL_Quit.invoke();
                return;
            }

            AtomicBoolean quit = new AtomicBoolean(false);
            long startTime = System.currentTimeMillis();

            MemorySegment event = arena.allocate(56);  // Size of SDL_Event structure

            while (!quit.get()) {
                while ((int) SDL_PollEvent.invoke(event) != 0) {
                    int type = event.get(JAVA_INT, 0);
                    if (type == SDL_QUIT) {
                        quit.set(true);
                    }
                    if (type == SDL_KEYDOWN) {
                        int key = event.get(JAVA_INT, 4);
                        if (key == SDLK_ESCAPE) {
                            quit.set(true);
                        }
                    }
                }

                long elapsedTime = System.currentTimeMillis() - startTime;
                if (elapsedTime > 2000) {
                    break;
                }

                SDL_RenderClear.invoke(renderer);
                SDL_RenderCopy.invoke(renderer, texture, MemorySegment.NULL, MemorySegment.NULL);
                SDL_RenderPresent.invoke(renderer);
                SDL_Delay.invoke(100);
            }

            SDL_DestroyTexture.invoke(texture);
            SDL_DestroyRenderer.invoke(renderer);
            SDL_DestroyWindow.invoke(window);
            SDL_Quit.invoke();
        }
    }

    private static String getError(MethodHandle SDL_GetError) {
        try {
            MemorySegment errorSegment = (MemorySegment) SDL_GetError.invoke();
            return segmentToString(errorSegment);
        } catch (Throwable e) {
            return "Unknown error";
        }
    }

    private static String segmentToString(MemorySegment segment) {
        int length = 0;
        while (segment.get(ValueLayout.JAVA_BYTE, length) != 0) {
            length++;
        }
        byte[] bytes = new byte[length];
        for (int i = 0; i < length; i++) {
            bytes[i] = segment.get(ValueLayout.JAVA_BYTE, i);
        }
        return new String(bytes, StandardCharsets.UTF_8);
    }

    private static MemorySegment allocateString(Arena arena, String str) {
        byte[] bytes = str.getBytes(StandardCharsets.UTF_8);
        MemorySegment segment = arena.allocate(bytes.length + 1);  // +1 for null terminator
        segment.asByteBuffer().put(bytes).put((byte) 0);
        return segment;
    }

    private static MethodHandle loadFunction(String name, FunctionDescriptor descriptor) {
        Optional<MemorySegment> address = lib.find(name);
        if (address.isPresent()) {
            return linker.downcallHandle(address.get(), descriptor);
        } else {
            System.err.println("Failed to load function: " + name);
            return null;
        }
    }
}
