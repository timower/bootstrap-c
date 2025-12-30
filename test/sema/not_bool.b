// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// CHECK: Expected bool!
func test() -> bool {
  return 1 || 2;
}
