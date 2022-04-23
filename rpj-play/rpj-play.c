#include <stdio.h>

int i = 12;

long l = 14;

const long lc = 16;

double d = 2.0;

int j;

struct rpj_struct {
  int i;
};

struct {
  int i;
  int j;
} rpj_struct_val;

struct rpj_struct2 {
  int j;
};

// Is this invalid?
struct rpj_struct_in_union {
  char c; 
};

union rpj_union {
  struct rpj_struct_in_union2 {
    float j;
  } siu;
  int i;
};

const char* s = "string";

int f(int i) { j = 2; }

int main(void) {
  printf("sizeof(struct rpj_struct) is %ld, sizeof(rpj_struct_val) is %ld, sizeof(struct rpj_struct2) is %ld\n", sizeof(struct rpj_struct), sizeof(rpj_struct_val), sizeof(struct rpj_struct2));
  printf("sizeof(struct rpj_struct_in_union) is %ld, sizeof(struct rpj_struct_in_union2) is %ld, sizeof(union rpj_union) is %ld\n", sizeof(struct rpj_struct_in_union), sizeof(struct rpj_struct_in_union2), sizeof(union rpj_union));
}
