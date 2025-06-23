// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Expected bool type
// CHECK: sema error: : Expected bool!

func main() -> i32 {
  let x = 42;
  if (x) {  // x is i32, not bool
    return 1;
  }
  return 0;
}