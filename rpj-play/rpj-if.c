#include <stdio.h>

int f(void) {
  int i = 2;

  if (i == 3) {
    i = 4;
  }
  else {
    i = 5;
  }

  return i;
}

int main(void) {
  printf("f() is %d\n", f());
}
