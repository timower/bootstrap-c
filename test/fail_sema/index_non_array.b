// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Index non-array type
// CHECK: sema error:  Index only works on arrays, or pointers to them.

func main() -> i32 {
  let x = 42;
  let y = x[0];  // Cannot index non-array
  return 0;
}