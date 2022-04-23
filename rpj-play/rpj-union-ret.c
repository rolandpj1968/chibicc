#include <stdio.h>

union u { char c; short s; double d; };

union u f1(void) {
  union u v1 = { .c = 'a' };

  return v1;
}

union u f2(void) {
  union u v2 = { .s = 1023 };

  return v2;
}

union u f3(void) {
  union u v3 = { .d = 7.89 };

  return v3;
}

int main(void) {
  union u l1 = f1();
  printf("l1.c = '%c'\n", l1.c);

  union u l2 = f2();
  printf("l2.s = '%d'\n", l2.s);

  union u l3 = f3();
  printf("l3.d = '%lf'\n", l3.d);
}
