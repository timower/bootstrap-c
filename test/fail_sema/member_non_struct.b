// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Member access on non-struct
// CHECK: sema error: Expected struct type for . expr

func main() -> i32 {
  let x = 42;
  let y = x.field;  // Cannot access member of non-struct
  return 0;
}