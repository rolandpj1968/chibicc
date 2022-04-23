struct s { char c; short s; double d; };

struct s v2;

struct s f(void) {
    return v2;
}

int main(void) {
  struct s l1 = f();
}
