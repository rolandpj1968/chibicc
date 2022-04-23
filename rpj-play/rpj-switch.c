#include <stdio.h>

int main(void) {
  int i = 0;
  for(unsigned j = 1; j < 5; j++) {

    switch(j*2) {
    case 1:
      i = 5;
      break;
    case 2:
      i = 4;
    case 3:
      i = 5;
      break;
    default:
      i = 9;
    }
    
    printf("j = %u, i = %d\n", j, i);
  }
  
  return i;
}
