// RUN: %bootstrap %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file=%t.ll
//
func main() -> i32 {
  // CHECK: store i1 1, ptr %alloc
  let trueV = true;

  // CHECK: store i1 0, ptr %alloc
  let falseV = false;

  if (trueV && falseV) {
    // CHECK: ret i32 1
    return 1;
  }
  if (trueV || falseV) {
    // CHECK: ret i32 0
    return 0;
  }

  // CHECK: ret i32 2
  return 2;
}
