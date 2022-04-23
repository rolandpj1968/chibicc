#include <stdio.h>

typedef int printf_t(const char*, ...);

static void test1(void) {
  unsigned int i1 = (unsigned int)((1l << 32)-1);
  unsigned int i2 = 2;
  unsigned int i3 = i1 + i2;

  unsigned long l3 = (unsigned long)i3;

  printf("i1 = %08x i2 = %08x i3 = %08x l3 = %016lx\n", i1, i2, i3, l3);

  printf_t* pprintf = &printf;//(1 < 2 ? &printf : 0);
  pprintf("i1 = %08x i2 = %08x i3 = %08x l3 = %016lx\n", i1, i2, i3, l3);
  
}

static void test2(void) {
  int i = -1;
  void* p = (void*)i;
  unsigned int u = 0xffffffff;
  void* p2 = (void*)u;
  printf("signed int %x -> p %p, unsigned int %x -> p %p\n", i, p, u, p2);
}

static void test3(void) {
  int i = -1;
  long l = (long)i;
  unsigned int u = 0xffffffff;
  long l2 = (long)u;
  printf("signed int %x -> long %lx, unsigned int %x -> long %lx\n", i, l, u, l2);
}

static void test4(void) {
  int i = -1;
  unsigned long l = (unsigned long)i;
  unsigned int u = 0xffffffff;
  unsigned long l2 = (unsigned long)u;
  printf("signed int %x -> unsigned long %lx, unsigned int %x -> unsigned long %lx\n", i, l, u, l2);
}

static void test5(void) {
  int i = -1;
  printf_t* p = (printf_t*)i;
  unsigned int u = 0xffffffff;
  printf_t* p2 = (printf_t*)u;
  printf("signed int %x -> fnp %p, unsigned int %x -> fnp %p\n", i, p, u, p2);
}

static void test6(void) {
  char c = -1;
  int i = (int)c;
  unsigned int u = (unsigned int)c;
  long l = (long)c;
  unsigned long ul = (unsigned long)c;

  printf("char c == -1 -> int %x unsigned int %x long %lx unsigned long %lx\n", i, u, l, ul);
}

static void test7(void) {
  unsigned char c = 0xff;
  int i = (int)c;
  unsigned int u = (unsigned int)c;
  long l = (long)c;
  unsigned long ul = (unsigned long)c;

  printf("unsigned char c == 0xff -> int %x unsigned int %x long %lx unsigned long %lx\n", i, u, l, ul);
}

int main(void) {
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
}
