#include <stdio.h>

struct rpj_struct1 {
  int i;
  float f;
};

struct rpj_struct1 rpj_struct1_var;
struct rpj_struct1 rpj_struct1_var2;

struct rpj_struct2 {
  int i;
  float f;
};

struct rpj_struct2 rpj_struct2_var;
struct rpj_struct2 rpj_struct2_var2 = { 1, 2.0f };

struct rpj_struct3 {
  struct rpj_struct2_inner1 {
    long l;
    double d;
  } ld;
  long l;
  double d;
};

struct rpj_struct3 rpj_struct3_var;
struct rpj_struct3 rpj_struct3_var2 = { { 1l, 2.0 }, 3l, 4.0 };

struct rpj_struct4 {
  struct {
    long l;
    double d;
  } ld;
  long l;
  double d;
};

struct rpj_struct4 rpj_struct4_var;

union rpj_union1 {
  int i;
  float f;
};

union rpj_union1 rpj_union1_var;

union {
  long l;
  double d;
} rpj_anon_union_var;

int f1(struct rpj_struct1 param1, struct { int i; } param2) {
  return 2;
}

struct rpj_array_struct1 {
  double d;
  char s[8];
};

struct rpj_array_struct1 rpj_array_struct1_var;

int main(void) {
  printf("All ok - just compiling\n");
}
