// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Function argument type mismatch
// CHECK: sema error:  Arg type mismatch

func test(x: i32) -> i32 {
  return x;
}

func main() -> i32 {
  let b: bool = true;
  let result = test(b);  // bool cannot convert to i32
  return 0;
}