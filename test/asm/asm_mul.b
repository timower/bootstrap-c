// RUN: %bootstrap -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: %cc %t.s -o %t
// RUN: %check-exit-code 42 %run %t

// Test multiplication with immediate values (requires loading both into registers)
// CHECK: {{_?}}main:
// CHECK:   mov w{{[0-9]+}}, #6
// CHECK:   mov w{{[0-9]+}}, #7
// CHECK:   mul w{{[0-9]+}}, w{{[0-9]+}}, w{{[0-9]+}}
// CHECK:   mov w0, w{{[0-9]+}}
// CHECK:   ret

func main() -> i32 {
  return 6 * 7;
}