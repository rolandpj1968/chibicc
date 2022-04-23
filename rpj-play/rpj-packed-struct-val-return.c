#include <stdio.h>

// Gnu headers stub this out deliberately if it's not
// /usr/include/x86_64-linux-gnu/sys/cdefs.h
/* #if !defined __GNUC__ || __GNUC__ < 2 */
/* # define __attribute__(xyz)	/\* Ignore *\/ */
/* #endif */

#ifdef __attribute__
# undef __attribute__
#endif

struct __attribute__((packed)) s { char c; short s; double d; };

struct s v2 = { 'c', 1023, 7.89 };

struct s f(void) {
    return v2;
}

int main(void) {
  struct s l1 = f();

  printf("l1 '%c' %d %lf\n", l1.c, l1.s, l1.d);
}
