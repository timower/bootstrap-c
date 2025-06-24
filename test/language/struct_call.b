// RUN: %bootstrap %s | lli
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
  return getFoo().y - 2 * getFoo().x;
}
