// RUN: %bootstrap %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file %t.ll
func main() -> i32 {
  let x = 1;

  // CHECK: shl i32 {{.*}}, 2
  x <<= 2;

  // CHECK: ashr i32 {{.*}}, 2
  x >>= 2;

  x = -x;

  x = ~x;

  return ~x;
}
