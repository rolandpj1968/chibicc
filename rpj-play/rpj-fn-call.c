extern int printf(const char*, ...);

static int f1(int i) {
  printf("i = %d\n", i);
}

int main(void) {
  f1(1);
  
  return 0;
}
