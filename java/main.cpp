#include <jni.h>
#include <SDL2/SDL.h>
#include <string>
#include <iostream>

using namespace std::string_literals;

void Happy() {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    SDL_Log("Unable to initialize SDL: %s", SDL_GetError());
    exit(1);
  }
}

JNIEXPORT void JNICALL Java_Main_Happy(JNIEnv *env, jobject obj) {
  Happy();
}

int main() {
  JavaVM *jvm;
  JNIEnv *env;

  JavaVMInitArgs vm_args;
  JavaVMOption options[1];

  std::string path1 = "-Djava.class.path=."s;
  //std::string path2 = "-Djava.class.path=./share/"s + std::string(PROJECT_NAME);

  options[0].optionString = (char*)(path1.c_str());
  //options[1].optionString = (char*)(path2.c_str());

  vm_args.version = JNI_VERSION_1_8;
  vm_args.nOptions = 1;
  vm_args.options = options;
  vm_args.ignoreUnrecognized = JNI_FALSE;

  // Create the JVM and obtain a JNIEnv
  JNI_CreateJavaVM(&jvm, (void **)&env, &vm_args);

  // Find the Main class
  jclass cls = env->FindClass("Main");
  if (cls == nullptr) {
    // Try to find Main.class in share/${PROJECT_NAME}
    std::string share_class_path = "./share/" + std::string(PROJECT_NAME) + "/Main.class";
    jclass share_cls = env->FindClass(share_class_path.c_str());
    if (share_cls == nullptr) {
      std::cerr << "Unable to find Main class" << std::endl;
      env->ExceptionDescribe();
      return 1;
    }
    cls = share_cls;
  }

  // Call the main function
  jmethodID mid = env->GetStaticMethodID(cls, "main", "([Ljava/lang/String;)V");
  if (mid == nullptr) {
    std::cerr << "Unable to find Main method" << std::endl;
    env->ExceptionDescribe();
    return 1;
  }

  env->CallStaticVoidMethod(cls, mid, nullptr);

  // Destroy the JVM
  jvm->DestroyJavaVM();

  return 0;
}
