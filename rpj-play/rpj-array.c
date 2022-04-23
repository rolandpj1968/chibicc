#include <stdio.h>

//char ga[8];
_Bool gba[17];

void f(void) {
  //char c1 = ga[7];
  _Bool b1 = gba[7];
  /* char la[15]; */

  /* char c2 = la[13]; */

  /* ga[3] = c1; */

  /* la[3] = c2; */

  /* return 777; */
}

int main(void) {
  f();
  
  printf("All good sizeof(gba) = %lu\n", sizeof(gba));
  
  return 0;
}
