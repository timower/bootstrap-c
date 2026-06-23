// RUN: %brio -emit-asm -emit-llvm %s | FileCheck %s
// CHECK: define i32 @main
func main() -> i32 {
  return 0;
}
