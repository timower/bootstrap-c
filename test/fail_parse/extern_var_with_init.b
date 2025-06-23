// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Extern variable with initializer
// CHECK: : Extern let cannot have init

extern let global_var = 42;  // extern variables cannot have initializers

func main() -> i32 {
  return 0;
}