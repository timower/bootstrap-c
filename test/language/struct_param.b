// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli

// CHECK: %struct.
// CHECK: alloca %struct.
// CHECK: getelementptr
// CHECK: call
struct Foo {
  x: i32;
  y: i32;
};

func foo(f: Foo) -> i32 {
  return f.x - 2 * f.y;
}

func bar(f: Foo*) -> i32 {
  return f->y - 2 * f->x;
}

func main() -> i32 {
  let x = Foo{
    x = 1,
    y = 2,
  };
  return bar(&x) + foo(Foo{
    x = 2,
    y = 1,
  });
}
