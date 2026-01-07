// RUN: %bootstrap %s | FileCheck %s
// CHECK: ret i32 12
const x = 11 + 1;

func main() -> i32 {
  return x;
}
