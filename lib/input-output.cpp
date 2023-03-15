#include <cstdio>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT int read_i32() {
  int x;
  scanf("%d", &x);
  return x;
}

extern "C" DLLEXPORT char* read_string() {
  char* ptr = new char[256]; // Hm I hope no one ever exeeds this limit
  scanf("%s", ptr);
  return ptr;
}

extern "C" DLLEXPORT void print_i32(int x) {
  printf("%d", x);
}

extern "C" DLLEXPORT void print_string(char* ptr) {
  printf("%s", ptr);
}

int main() {
  print_i32(read_i32());
  print_string("\n");
  print_string(read_string());
}
