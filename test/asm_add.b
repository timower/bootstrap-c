// RUN: %bootstrap -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: %cc %t.s -o %t
// RUN: %check-exit-code 8 %run %t

// Test addition with immediate values
// CHECK: {{_?}}main:
// CHECK:   mov w{{[0-9]+}}, #5
// CHECK:   add w{{[0-9]+}}, w{{[0-9]+}}, #3
// CHECK:   mov w0, w{{[0-9]+}}
// CHECK:   ret

func main() -> i32 {
  return 5 + 3;
}