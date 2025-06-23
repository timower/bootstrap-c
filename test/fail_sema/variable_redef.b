// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Variable redefinition error
// CHECK: sema error: Variable redef

func main() -> i32 {
  let x = 10;
  let x = 20;  // Redefinition of x
  return 0;
}