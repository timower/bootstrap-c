// RUN: not %brio %s -o %t.ll 2>&1 | FileCheck %s
// CHECK: irgen fail: Multiple default
func main() -> i32 {
  let x = 0;
  switch (x) {
    default:
      return 1;
    default:
      return 2;
  }
  return 0;
}
