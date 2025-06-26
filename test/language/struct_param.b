// RUN: %bootstrap %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file=%t.ll
struct Foo {
  x: i32;
  y: i32;
};

func foo(f: Foo) -> i32 {
  // CHECK: %struct.Foo = type
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
