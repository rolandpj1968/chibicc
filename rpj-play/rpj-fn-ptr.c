#include <stdio.h>

typedef int printf_t(const char*, ...);

void main(void) {
  unsigned int i1 = (unsigned int)((1l << 32)-1);
  unsigned int i2 = 2;
  unsigned int i3 = i1 + i2;

  unsigned long l3 = (unsigned long)i3;

  //printf_t* pprintf = &printf;//(1 < 2 ? &printf : 0);
  printf_t* pprintf = (1 < 2 ? &printf : 0);
  pprintf("i1 = %08x i2 = %08x i3 = %08x l3 = %016lx\n", i1, i2, i3, l3);
  
}
