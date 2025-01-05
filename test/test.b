import libc;

enum Foo {
  Bar,
  Baz,
};

struct S {
  i32 x;
};

u16 test() {
  enum Foo x = Foo::Bar;

  switch (x) {
  case Foo::Bar:
    return 1;
  case Foo::Baz:
    return 2;
  }

  return x as u16;
}
