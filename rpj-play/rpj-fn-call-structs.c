#include <stdio.h>

struct rpj_struct1 { int i; float j; };
struct rpj_struct2 { int i; float j; };

int f(void) {
  if(1) {
    struct rpj_struct1 f2(void);
    struct rpj_struct1 v = f2();
  }

  if(0) {
    extern void f3(struct rpj_struct2 p);
    //extern void f3();
    struct rpj_struct2 v;
    f3(v);
  }
}

struct rpj_struct1 f2(void) {
  printf("f2...\n");
  struct rpj_struct1 s = { 1, 2.0f };
  return s;
}

int main(void) {
  int i = f();
}
