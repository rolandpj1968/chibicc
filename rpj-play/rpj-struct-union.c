#include <stdio.h>

typedef int t;
union u { t m1; long m2; double d3; char s4[37]; };

struct s { union u m1; char c; };

struct s v1, v2;

void f(void) {
	v1 = v2;
}
void g(void) {
	extern int cond;
	struct s v = cond ? v1 : v2;
}

union u h(int i) {
    struct s v = {};
    return v.m1;
}

int main(void) {
  union u u1 = h(1);
  printf("u1.m1 = %d\n", u1.m1);
}

int cond = 2;
