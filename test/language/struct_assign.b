// RUN: %bootstrap %s -o %t.ll
// RUN: FileCheck %s < %t.ll
// RUN: lli %t.ll

// CHECK: %struct.Foo = type
// CHECK: %struct.Bar = type
// CHECK: alloca %struct.Bar
// CHECK: getelementptr
// CHECK: ret i32
struct Bar {
  z: i32;
  w: Foo;
};

struct Foo {
  x: i32;
  y: i32;
  ptr: Bar*;
};


func main() -> i32 {
  let b = Bar{
    z = 55,
  };
  let x = Foo{
    x = 2,
    y = 3,
    ptr = &b,
  };
  let ptr = &x;
  ptr->ptr->w = *ptr;

  return b.w.ptr->z - 55;
}
