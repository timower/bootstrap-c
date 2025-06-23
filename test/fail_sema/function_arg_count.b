// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Function argument count mismatch
// CHECK: sema error: Function call arg length mismatch

func test(x: i32, y: i32) -> i32 {
  return x + y;
}

func main() -> i32 {
  let result = test(42);  // Missing second argument
  return 0;
}