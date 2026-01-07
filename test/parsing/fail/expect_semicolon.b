// RUN: cat %s | not %bootstrap - 2>&1 | FileCheck %s
// CHECK: Expected: ;
func test() {
  return 12
}
