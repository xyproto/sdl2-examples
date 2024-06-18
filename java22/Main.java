import static java.lang.foreign.ValueLayout.*;
import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Optional;

public class Main {
    private static final Linker linker = Linker.nativeLinker();
    private static SymbolLookup lib;

    // Constants from SDL
    private static final int SDL_INIT_VIDEO = 0x00000020;
    private static final int SDL_WINDOWPOS_UNDEFINED = 0x1FFF0000;
    private static final int SDL_WINDOW_SHOWN = 0x00000004;
    private static final int SDL_RENDERER_ACCELERATED = 0x00000002;
    private static final int SDL_RENDERER_PRESENTVSYNC = 0x00000004;

    public static void main(String[] args) {
        // Ensure the code runs on the main thread
        if (!java.awt.EventQueue.isDispatchThread()) {
            java.awt.EventQueue.invokeLater(() -> {
                try {
                    runSDL();
                } catch (Throwable e) {
                    e.printStackTrace();
                }
            });
        } else {
            try {
                runSDL();
            } catch (Throwable e) {
                e.printStackTrace();
            }
        }
    }

    private static void runSDL() throws Throwable {
        String libPath = System.getProperty("sdl2.library.path");
        if (libPath == null || libPath.isEmpty()) {
            throw new IllegalArgumentException("Library path not specified. Use -Dsdl2.library.path=<path>");
        }

        try (Arena arena = Arena.ofConfined()) {
            lib = SymbolLookup.libraryLookup(Path.of(libPath), arena);
            System.out.println("Successfully created SymbolLookup for SDL2 library.");

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

            if (SDL_Init == null || SDL_Quit == null || SDL_CreateWindow == null || SDL_CreateRenderer == null || SDL_CreateTextureFromSurface == null ||
                SDL_DestroyRenderer == null || SDL_DestroyWindow == null || SDL_FreeSurface == null || SDL_DestroyTexture == null ||
                SDL_RenderClear == null || SDL_RenderCopy == null || SDL_RenderPresent == null || SDL_LoadBMP_RW == null || SDL_RWFromFile == null || SDL_GetError == null) {
                System.err.println("Failed to load one or more SDL functions");
                return;
            }

            int result = (int) SDL_Init.invoke(SDL_INIT_VIDEO);
            if (result != 0) {
                System.err.println("SDL_Init failed with error code: " + result);
                return;
            }
            System.out.println("SDL_Init succeeded");

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

            for (int i = 0; i < 20; i++) {
                SDL_RenderClear.invoke(renderer);
                SDL_RenderCopy.invoke(renderer, texture, MemorySegment.NULL, MemorySegment.NULL);
                SDL_RenderPresent.invoke(renderer);
                Thread.sleep(100);
            }

            SDL_DestroyTexture.invoke(texture);
            SDL_DestroyRenderer.invoke(renderer);
            SDL_DestroyWindow.invoke(window);
            SDL_Quit.invoke();

            System.out.println("SDL_Quit called successfully.");
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
            System.out.println("Loaded function: " + name);
            return linker.downcallHandle(address.get(), descriptor);
        } else {
            System.err.println("Failed to load function: " + name);
            return null;
        }
    }
}
