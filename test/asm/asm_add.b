// RUN: %bootstrap -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: %cc %t.s -o %t
// RUN: %check-exit-code 11 %run %t
//
// Test addition with immediate values
// CHECK: {{_?}}main:
// CHECK:   mov r{{[0-9]+}}, #5
// CHECK:   add r{{[0-9]+}}, r{{[0-9]+}}, #3
// CHECK:   mov r0, r{{[0-9]+}}
// CHECK:   bx lr
//
func main() -> i32 {
  return (5 + 3) + (1  + 2);
}
