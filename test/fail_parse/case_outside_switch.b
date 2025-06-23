// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Case outside switch
// CHECK: : Unknow primary expression

func main() -> i32 {
  switch (1) {
    case 1:
      return 1;
  }
  case 2:  // Case outside switch - should cause sema error
    return 2;
  return 0;
}