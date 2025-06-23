// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Unknown type
// CHECK: : Unknown type

func main() -> i32 {
  let x: if = 42;  // 'if' is not a valid type token
  return 0;
}