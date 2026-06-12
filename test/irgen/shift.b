// RUN: %brio %s | FileCheck %s
func foo(x: i32, y: u32) {
  // CHECK: ashr i32
  let a = x >> 2;

  // CHECK: lshr i32
  let b = y >> 2;
}
