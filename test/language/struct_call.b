// RUN: %compile-and-run %s %t.ll
// RUN: FileCheck %s --input-file=%t.ll
struct Foo {
  x: i32;
  y: i32;
};

func getFoo() -> Foo {
  return Foo{
    x = 1,
    y = 2,
  };
}

func main() -> i32 {
  // CHECK: call %struct.Foo () @getFoo()
  return getFoo().y - 2 * getFoo().x;
}
