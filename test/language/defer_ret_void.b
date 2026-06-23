// RUN: %compile-and-run %s
import stdlib.libc;

func test(x: i32) {
  defer puts("Hello");

  if (x == 0) {
    return;
  }
}

func main() -> i32 {
  test(0);
  test(1);
  return 0;
}
