union u { char c; short s; double d; };

union u v2;

union u f(void) {
    return v2;
}

int main(void) {
  union u l1 = f();
}
