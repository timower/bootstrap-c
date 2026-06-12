// RUN: %brio %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file %t.ll
func main() -> i32 {
  let x = 1;

  // CHECK: shl i32 {{.*}}, 2
  x <<= 2;

  // CHECK: ashr i32 {{.*}}, 2
  x >>= 2;

  // CHECK: sub i32 0, {{.*}}
  x = -x;

  // CHECK: xor i32 {{.*}}, -1
  return ~x;
}
