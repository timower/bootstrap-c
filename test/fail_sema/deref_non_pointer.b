// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Dereference non-pointer
// CHECK: sema error: Expected pointer type for *

func main() -> i32 {
  let x = 42;
  let y = *x;  // Cannot dereference non-pointer
  return 0;
}