// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Unknown token
// CHECK: : Unknown token

func main() -> i32 {
  let x = 42 § 10;  // § is not a valid token
  return 0;
}