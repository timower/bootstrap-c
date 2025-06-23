// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Invalid integer literal
// CHECK: : Invalid integer

func main() -> i32 {
  let x = 0b2;  // '2' is not a valid binary digit
  return 0;
}