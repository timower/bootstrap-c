// RUN: %brio %s | FileCheck %s
enum X {
  A,
  B,
}


// CHECK: define i32 @x
func x() -> X {
  switch (12) {
    default:
      // CHECK-DAG: ret i32 1
      return X::B;
  }
  // CHECK-DAG: ret i32 0
}
