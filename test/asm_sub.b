// RUN: %bootstrap -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: cc %t.s -o %t
// RUN: %check-exit-code 6 %t

// Test subtraction with immediate values  
// CHECK: _main:
// CHECK:   mov w{{[0-9]+}}, #10
// CHECK:   sub w{{[0-9]+}}, w{{[0-9]+}}, #4
// CHECK:   mov w0, w{{[0-9]+}}
// CHECK:   ret

func main() -> i32 {
  return 10 - 4;
}