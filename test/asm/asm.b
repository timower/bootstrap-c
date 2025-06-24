// RUN: %bootstrap -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: %cc %t.s -o %t
// RUN: %run %t

// CHECK: {{_?}}helper:
// CHECK:   mov w0, #123
// CHECK:   ret

// CHECK: {{_?}}main:
// CHECK:   mov w0, #0
// CHECK:   ret

func main() -> i32 {
  return 0;
}

func helper() -> i32 {
  return 123;
}
