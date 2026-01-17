// RUN: %bootstrap -sema-lsp %s 2>&1 | FileCheck %s
// RUN: %bootstrap -sema %s 2>&1 | FileCheck --allow-empty --check-prefix=DIS %s
//
// CHECK: OK!
// DIS-NOT: OK!
func main() -> i32 {
  return 0;
}
