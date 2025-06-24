// RUN: %bootstrap %s | lli
struct Foo {
  x: i32;
  y: i32;
  z: Bar;
};

struct Bar {
  z: i32;
};

func main() -> i32 {
  let s = Foo{
    z = Bar{
      z = 3,
    },
    y = 2,
    x = 1,
  };

  return s.z.z - s.x - s.y;
}
