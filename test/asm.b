// RUN: %bootstrap -emit-asm %s -o %t.s
// RUN: cat %t.s | FileCheck %s
// RUN: cc %t.s -o %t
// RUN: %t

// CHECK: _helper:
// CHECK:   mov w0, #123
// CHECK:   ret

// CHECK: _main:
// CHECK:   mov w0, #0
// CHECK:   ret

func main() -> i32 {
  return 0;
}

func helper() -> i32 {
  return 123;
}
