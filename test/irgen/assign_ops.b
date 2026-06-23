// RUN: %compile-and-run %s %t.ll
// RUN: FileCheck %s --input-file %t.ll
extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
  let x = 10;
  let y = 3;
  let mask = 0b1010;

  printf("Initial: x=%d, y=%d, mask=%d\n", x, y, mask);

  // Test addition assignment
  // CHECK: load i32, ptr %alloc
  // CHECK: add i32 {{.*}}, 5
  // CHECK: store i32 {{.*}}, ptr %alloc
  x += 5;

  // Test subtraction assignment
  // CHECK: load i32, ptr %alloc
  // CHECK: sub i32 {{.*}}, 3
  // CHECK: store i32 {{.*}}, ptr %alloc
  x -= 3;

  // Test multiplication assignment
  // CHECK: load i32, ptr %alloc
  // CHECK: mul i32 {{.*}}, 4
  // CHECK: store i32 {{.*}}, ptr %alloc
  y *= 4;

  // Test division assignment
  // CHECK: load i32, ptr %alloc
  // CHECK: sdiv i32 {{.*}}, 2
  // CHECK: store i32 {{.*}}, ptr %alloc
  y /= 2;

  // Test modulo assignment
  // CHECK: load i32, ptr %alloc
  // CHECK: srem i32 {{.*}}, 5
  // CHECK: store i32 {{.*}}, ptr %alloc
  x %= 5;

  // Test left shift assignment
  // CHECK: load i32, ptr %alloc
  // CHECK: shl i32 {{.*}}, 2
  // CHECK: store i32 {{.*}}, ptr %alloc
  mask <<= 2;

  // Test right shift assignment
  // CHECK: load i32, ptr %alloc
  // CHECK: ashr i32 {{.*}}, 1
  // CHECK: store i32 {{.*}}, ptr %alloc
  mask >>= 1;

  // Test bitwise AND assignment
  // CHECK: load i32, ptr %alloc
  // CHECK: and i32 {{.*}}, 15
  // CHECK: store i32 {{.*}}, ptr %alloc
  mask &= 15;

  // Test bitwise XOR assignment
  // CHECK: load i32, ptr %alloc
  // CHECK: xor i32 {{.*}}, 7
  // CHECK: store i32 {{.*}}, ptr %alloc
  mask ^= 7;

  // Test bitwise OR assignment
  // CHECK: load i32, ptr %alloc
  // CHECK: or i32 {{.*}}, 16
  // CHECK: store i32 {{.*}}, ptr %alloc
  mask |= 16;

  printf("Final: x=%d, y=%d, mask=%d\n", x, y, mask);

  return 0;
}
