// RUN: %bootstrap %s | FileCheck %s
// CHECK: define [2 x i32] @foo()
func foo() -> i32[2] {
  return [ 1, 2 ];
}
