// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Empty case statement
// CHECK: : Empty case not allowed

func main() -> i32 {
  let x = 1;
  switch (x) {
    case 1:  // empty case - should have break or statements
    case 2:
      return 2;
  }
  return 0;
}
