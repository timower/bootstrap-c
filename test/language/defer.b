// RUN: %brio %s -o %t.ll
// RUN: lli %t.ll | FileCheck %s
// CHECK: x1: 3
// CHECK: x2: 5
// CHECK: x3: 12
extern func printf(s: i8*, ...) -> i32;


func test(x: i32*, do: bool) {
  while (true) {
    defer *x += 2;
    if (do) {
      break;
    }
    defer *x += 1;
    break;
  }
}


func main() -> i32 {
  let x = 0;
  test(&x, false);
  printf("x1: %d\n", x);

  test(&x, true);
  printf("x2: %d\n", x);

  x = 0;

  defer printf("x3: %d\n", x);
  defer x = 12;
  return 0;
}
