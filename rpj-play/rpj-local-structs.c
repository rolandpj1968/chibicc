int main() {
  int i = 2;

  if (i) {
    struct local_struct { int i; int j; } v1;
  }
  else {
    struct local_struct { int f; int g; } v2;
  }
}
