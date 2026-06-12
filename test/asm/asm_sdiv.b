// RUN: %brio -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: %cc %t.s -o %t
// RUN: %check-exit-code 5 %run %t
//
// Test signed division with immediate values (requires loading both into registers)
// CHECK: {{_?}}main:
// CHECK:   mov r{{[0-9]+}}, #10
// CHECK:   add r{{[0-9]+}}, r{{[0-9]+}}, #10
// CHECK:   mov r{{[0-9]+}}, #4
// CHECK:   sdiv r{{[0-9]+}}, r{{[0-9]+}}, r{{[0-9]+}}
// CHECK:   mov r0, r{{[0-9]+}}
// CHECK:   bx lr
//
func main() -> i32 {
  return (10 + 10) / 4;
}
