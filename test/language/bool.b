// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli

// CHECK: alloca i1
// CHECK: store i1 true
// CHECK: store i1 false
// CHECK: icmp
// CHECK: and i1
// CHECK: or i1
func main() -> i32 {
  let trueV = true;
  let falseV = false;
  if (trueV && falseV) {
    return 1;
  }
  if (trueV || falseV) {
    return 0;
  }
  return 2;
}
