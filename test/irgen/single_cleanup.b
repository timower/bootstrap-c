// RUN: %brio %s | FileCheck %s
//
// CHECK: %cleanupslot{{.*}} = alloca
// CHECK-NOT: %cleanupslot{{.*}} = alloca
//
func test(x: i32*, do: bool) {
  while (true) {
    defer *x += 2;
    if (do) {
      break;
    }
    defer *x += 1;
    break;
  }
}
