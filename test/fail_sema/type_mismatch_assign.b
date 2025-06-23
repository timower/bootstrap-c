// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Assignment type mismatch
// CHECK: sema error: : Assign doesn't match

func main() -> i32 {
  let x: i32 = 10;
  let y: bool = true;
  x = y;  // Cannot assign bool to i32
  return 0;
}