// RUN: %bootstrap %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file %t.ll
extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
  let x = 5;
  let y = 10;
  let flag = true;

  // Test unary minus
  // CHECK: sub i32 0, {{.*}}
  let neg_x = -x;

  // Test bitwise NOT
  // CHECK: xor i32 {{.*}}, -1
  let not_x = ~x;

  // Test logical NOT (implemented as icmp eq with 0)
  // CHECK: icmp eq i1 {{.*}}, 0
  let not_flag = !flag;

  // Test unary plus (implemented as add with 1)
  let pos_x = +x;

  // Test pre-increment
  // CHECK: add i32 {{.*}}, 1
  // CHECK: store i32 {{.*}}, ptr %alloc3
  ++y;

  // Test post-increment
  // CHECK: add i32 {{.*}}, 1
  // CHECK: store i32 {{.*}}, ptr %alloc1
  x++;

  // Test pre-decrement
  // CHECK: add i32 {{.*}}, -1
  // CHECK: store i32 {{.*}}, ptr %alloc3
  --y;

  // Test post-decrement
  // CHECK: add i32 {{.*}}, -1
  // CHECK: store i32 {{.*}}, ptr %alloc1
  x--;

  // Test address-of and dereference
  // CHECK: store ptr %alloc1, ptr {{.*}}
  let ptr = &x;

  // CHECK: load ptr, ptr {{.*}}
  // CHECK: load i32, ptr {{.*}}
  let deref_val = *ptr;

  printf(
      "Results: neg=%d, not=%d, pos=%d, x=%d, y=%d, deref=%d\n",
      neg_x,
      not_x,
      pos_x,
      x,
      y,
      deref_val);

  return 0;
}
