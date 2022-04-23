#include <stdio.h>

int f(int p1, double p2, int p3, double p4, int p5, double p6) {
  int v1 = 2;
  float v2 = 2.0f;
  char c;
  short d;
  struct { int i; double j; unsigned long l; } s;

  {
    long p1 = 2;
    char c = 'a';
  }
}

int main(void) {
  printf("All OK - just have to compile this\n");
}
