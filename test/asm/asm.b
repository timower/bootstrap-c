// RUN: %bootstrap -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: %cc %t.s -o %t
// RUN: %run %t
//
// CHECK: {{_?}}helper:
// CHECK:   mov r0, #123
// CHECK:   bx lr
//
// CHECK: {{_?}}main:
// CHECK:   mov r0, #0
// CHECK:   bx lr
//
func main() -> i32 {
  return 0;
}

func helper() -> i32 {
  return 123;
}
