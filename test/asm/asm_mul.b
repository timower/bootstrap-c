// RUN: %brio -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: %cc %t.s -o %t
// RUN: %check-exit-code 42 %run %t
//
// Test multiplication with immediate values (requires loading both into registers)
// CHECK: {{_?}}main:
// CHECK:   mov r{{[0-9]+}}, #6
// CHECK:   mov r{{[0-9]+}}, #7
// CHECK:   mul r{{[0-9]+}}, r{{[0-9]+}}, r{{[0-9]+}}
// CHECK:   mov r0, r{{[0-9]+}}
// CHECK:   bx lr
//
func main() -> i32 {
  return 6 * 7;
}
