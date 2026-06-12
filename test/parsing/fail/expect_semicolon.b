// RUN: cat %s | not %brio - 2>&1 | FileCheck %s
// CHECK: Expected: ;
func test() {
  return 12
}
