// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Call non-function type
// CHECK: sema error: Must call function type

func main() -> i32 {
  let x = 42;
  let result = x();  // x is not a function
  return 0;
}