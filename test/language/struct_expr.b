// RUN: %bootstrap %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file=%t.ll
//
// CHECK-DAG: %struct.Foo = type
struct Foo {
  x: i32;
  y: i32;
  z: Bar;
};


// CHECK-DAG: %struct.Bar = type
struct Bar {
  z: i32;
};

func main() -> i32 {
  let s = Foo {
    z = Bar {
      z = 3,
    },
    y = 2,
    x = 1,
  };

  return s.z.z - s.x - s.y;
}
