// RUN: not %brio -sema %s 2>&1 | FileCheck %s
// Test extern const error (gets caught at parser level)

// CHECK: Expected func or let
extern const INVALID_EXTERN_CONST: i32 = 100;

func main() -> i32 {
    return 0;
}