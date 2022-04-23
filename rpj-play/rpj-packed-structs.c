#include <stdio.h>
#include <stddef.h>

// Gnu headers stub this out deliberately if it's not
// /usr/include/x86_64-linux-gnu/sys/cdefs.h
/* #if !defined __GNUC__ || __GNUC__ < 2 */
/* # define __attribute__(xyz)	/\* Ignore *\/ */
/* #endif */

#ifdef __attribute__
# undef __attribute__
#endif

//struct __attribute__((__packed__)) packed_struct {
struct __attribute__((packed)) packed_struct {
  char c;
  int i;
  double d;
};

int test1(struct packed_struct* packs) {
  printf("sizeof(*packs) %ld\n", sizeof(*packs));
  printf("offsetof(struct packed_struct, i) %ld\n", offsetof(struct packed_struct, i));
}

int main(void) {
  struct packed_struct packs = { '1', 1, 1.0 };
  test1(&packs);
}
