// RUN: %compile-and-run %s
//
struct Bar {
  x: i32;
}

func test() -> Bar {
  let x = Bar {
    x = 1,
  };
  defer x.x = 0;
  return x;
}

func main() -> i32 {
  return test().x - 1;
}
