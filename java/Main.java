class Main {
  private static native void Happy();

  public static void main(String[] args) {
    System.out.println("Calling SDL_Init");
    Happy();
    System.out.println("Done calling SDL_Init");
  }
}
