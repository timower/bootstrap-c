// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// CHECK: Expected: ;
func test() {
  return 12
}
