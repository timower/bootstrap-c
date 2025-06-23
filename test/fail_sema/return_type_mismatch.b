// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Return type mismatch
// CHECK: sema error: Return type mismatch

func test() -> i32* {
  return 42;  // i32 cannot convert to i32*
}

func main() -> i32 {
  return 0;
}