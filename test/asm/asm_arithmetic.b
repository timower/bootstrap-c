// RUN: %bootstrap -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: %cc %t.s -o %t
// RUN: %run %t

// Test subtraction with immediate values
// CHECK: {{_?}}main:
// CHECK:   mov w{{[0-9]+}}, #2
// CHECK:   sub w{{[0-9]+}}, w{{[0-9]+}}, #2
// CHECK:   mov w0, w{{[0-9]+}}
// CHECK:   ret

func main() -> i32 {
  return 2 - 2;
}
