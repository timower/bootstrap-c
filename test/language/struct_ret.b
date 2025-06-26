// RUN: %bootstrap %s -o %t.ll
// RUN: lli %t.ll
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
  let f = getFoo();
  let ptr = &f;
  return f.y - 2 * ptr->x;
}
