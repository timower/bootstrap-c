// RUN: %brio -sema  %s 2>&1 | FileCheck %s --allow-empty --check-prefix NODBG
//
// RUN: %brio -sema -debug %s 2>&1 | FileCheck %s --check-prefix DBG
// RUN: %brio -sema-lsp -debug %s 2>&1 | FileCheck %s --check-prefix DBG
//
// Test debug flag functionality to increase coverage
// NODBG-NOT: Begin sema
//
// DBG: Begin sema
// DBG: End sema
// DBG-NOT: Begin irgen
func main() -> i32 {
  return 0;
}
