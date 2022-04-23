#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <stdio.h>

int main(void) {
  assert(1 < 2001);
  printf("Good, 1 < 2001\n");
  assert(5.39 > 1005.39);
  printf("Oh dear, 5.39 > 1005.39\n");
}
