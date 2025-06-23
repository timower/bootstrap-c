// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Ternary operator type mismatch
// CHECK: sema error: ?: lhs and rhs should have same type

func main() -> i32 {
  let condition = true;
  let x = condition ? 42 : true;  // i32 vs bool
  return 0;
}