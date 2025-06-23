// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Const with non-int initializer
// CHECK: sema error: Const decl must have an int init

const BAD_CONST = "string";  // string not allowed for const

func main() -> i32 {
  return 0;
}