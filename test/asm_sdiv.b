// RUN: %bootstrap -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: cc %t.s -o %t
// RUN: %check-exit-code 5 %t

// Test signed division with immediate values (requires loading both into registers)
// CHECK: _main:
// CHECK:   mov w{{[0-9]+}}, #20
// CHECK:   mov w{{[0-9]+}}, #4
// CHECK:   sdiv w{{[0-9]+}}, w{{[0-9]+}}, w{{[0-9]+}}
// CHECK:   mov w0, w{{[0-9]+}}
// CHECK:   ret

func main() -> i32 {
  return 20 / 4;
}