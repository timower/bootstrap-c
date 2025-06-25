// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli

// CHECK: define i32 @main()
// CHECK: entry.0:
// CHECK: ret i32 0
func main() -> i32 {
  return 0;
}
