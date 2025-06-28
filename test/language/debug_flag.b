// RUN: %bootstrap -sema -debug %s 2>&1 | FileCheck %s
// Test debug flag functionality to increase coverage
// CHECK: Begin sema
// CHECK: End sema
func main() -> i32 {
  return 0;
}
