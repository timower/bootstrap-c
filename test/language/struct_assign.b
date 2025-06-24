// RUN: %bootstrap %s | lli
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
