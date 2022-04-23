#include <stdio.h>

void g(void);

int f(char c1, char c2) {
  switch (c1 + c2) {
  case 'a'+'b':
    g();
  }
}

void g(void) {
  printf("Running g()\n");
}

int main(void) {
  f('a', 'b');
  f('b', 'a');
  f('c', 'd');
}
  
