// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Extern const declaration
// CHECK: : Expected func or let

extern const GLOBAL_CONST = 100;  // extern const is not allowed

func main() -> i32 {
  return 0;
}