// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Variable not found in scope
// CHECK: sema error: Couldn't find variable in scope

func main() -> i32 {
  let x = undefined_var;  // undefined_var not declared
  return 0;
}