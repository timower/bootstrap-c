// RUN: %bootstrap -sema-lsp %s 2>&1 | FileCheck %s
// CHECK: OK!
func main() -> i32 {
  return 0;
}
