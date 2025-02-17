struct Bar {
  enum { A, B, C } foo;

  struct Foo {
    char y;
    int x;
    char z;
  };

  int x;
};

struct Foo foo;

int fop() {
  enum { C, B, A } bar;
  return sizeof(struct Foo);
}

int main() { return A; }
