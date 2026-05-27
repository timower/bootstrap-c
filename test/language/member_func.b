// RUN: %bootstrap %s | lli | FileCheck %s
// CHECK: Hello
// CHECK: World
// CHECK: !
struct Foo {
  bar: func*();
}

struct Bar {}

union Buz {
  A {
    foo: func*();
  }
}

extern func puts(s: i8*);

func hello() {
  puts("Hello");
}

func Foo::world(this: Foo*) {
  puts("World");
}

func Bar::world(this: Bar*) {
  puts("!");
}

func main() -> i32 {
  let x = Foo {
    bar = &hello,
  };

  x.bar();

  x.world();
  let y = Bar {};
  let z = &y;
  z->world();

  return 0;
}
