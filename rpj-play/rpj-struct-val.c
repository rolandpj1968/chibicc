#include <stdio.h>
//extern int printf(const char*, ...);

struct s { int i; double d; };

int main(void) {
  struct s s1 = { 1, 1.0 };
  struct s s2 = { 2, 2.0 };

  printf("start - s1 { %d, %lf } s2 { %d, %lf }\n", s1.i, s1.d, s2.i, s2.d);

  struct s s3 = s1;

  printf("test1 - s1 { %d, %lf } s2 { %d, %lf } s3 { %d, %lf }\n", s1.i, s1.d, s2.i, s2.d, s3.i, s3.d);
  
  s3.i = 3;
  s3.d = 3.0;

  printf("test2 - s1 { %d, %lf } s2 { %d, %lf } s3 { %d, %lf }\n", s1.i, s1.d, s2.i, s2.d, s3.i, s3.d);
  
}
