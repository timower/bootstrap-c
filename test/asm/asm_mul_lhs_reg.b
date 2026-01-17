// RUN: %bootstrap -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: %cc %t.s -o %t
// RUN: %check-exit-code 42 %run %t
//
// Test multiplication with immediate values (requires loading both into registers)
// CHECK: {{_?}}main:
// CHECK:   mov r[[REG1:[0-9]+]], #3
// CHECK:   add r[[REG2:[0-9]+]], r[[REG1]], #3
// CHECK:   mov r12, #7
// CHECK:   mul r[[REG3:[0-9]+]], r[[REG2]], r12
// CHECK:   mov r0, r[[REG3]]
// CHECK:   bx lr
//
func main() -> i32 {
  return (3 + 3) * 7;
}
