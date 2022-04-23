#include <stdio.h>

struct s { char c; short s; double d; };

struct s v2 = { 'c', 1023, 7.89 };

struct s f(void) {
    return v2;
}

int main(void) {
  struct s l1 = f();

  printf("l1 '%c' %d %lf\n", l1.c, l1.s, l1.d);
}
