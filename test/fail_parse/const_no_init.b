// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Const without initializer
// CHECK: : Expected: =

func main() -> i32 {
  const LOCAL_CONST;  // const must be initialized - should cause sema error
  return 0;
}