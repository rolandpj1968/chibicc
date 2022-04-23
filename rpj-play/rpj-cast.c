#include <stdio.h>

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

typedef signed char i8;
typedef signed short i16;
typedef signed int i32;
typedef signed long i64;

int main() {
  u16 v_u16 = 255;

  printf("u16 v_u16 = %d: (u8)v_u16 = %d, (i8)v_u16 = %d, (u16)v_u16 = %d, (i16)v_u16 = %d\n", v_u16, (u8)v_u16, (i8)v_u16, (u16)v_u16, (i16)v_u16);

  u8 v_u8 = 255;

  printf("u8 v_u8 = %d: (u8)v_u8 = %d, (i8)v_u8 = %d, (u16)v_u8 = %d, (i16)v_u8 = %d\n", v_u8, (u8)v_u8, (i8)v_u8, (u16)v_u8, (i16)v_u8);

  printf("\n");
  
  v_u16 = 257;

  printf("u16 v_u16 = %d: (u8)v_u16 = %d, (i8)v_u16 = %d, (u16)v_u16 = %d, (i16)v_u16 = %d\n", v_u16, (u8)v_u16, (i8)v_u16, (u16)v_u16, (i16)v_u16);

  i16 v_i16 = 257;

  printf("i16 v_i16 = %d: (u8)v_i16 = %d, (i8)v_i16 = %d, (u16)v_i16 = %d, (i16)v_i16 = %d\n", v_i16, (u8)v_i16, (i8)v_i16, (u16)v_i16, (i16)v_i16);

  v_i16 = -1;

  printf("i16 v_i16 = %d: (u8)v_i16 = %d, (i8)v_i16 = %d, (u16)v_i16 = %d, (i16)v_i16 = %d\n", v_i16, (u8)v_i16, (i8)v_i16, (u16)v_i16, (i16)v_i16);
  
}
