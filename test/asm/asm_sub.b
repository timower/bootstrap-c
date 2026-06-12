// RUN: %brio -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: %cc %t.s -o %t
// RUN: %run %t
//
// Test subtraction with immediate values
// CHECK: {{_?}}main:
// CHECK:   mov r{{[0-9]+}}, #2
// CHECK:   sub r{{[0-9]+}}, r{{[0-9]+}}, #2
// CHECK:   mov r0, r{{[0-9]+}}
// CHECK:   bx lr
//
func main() -> i32 {
  return 2 - 2;
}
