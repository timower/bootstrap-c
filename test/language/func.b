// RUN: %bootstrap %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file=%t.ll
func main() -> i32 {
  // CHECK: define i32 @main() {
  // CHECK: entry.0.{{.*}}:
  return 0;
}
