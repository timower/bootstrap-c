// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Return type should be void
// CHECK: sema error: Return type should be void

func test() -> i32 {
  return;  // Empty return in non-void function
}

func main() -> i32 {
  return 0;
}