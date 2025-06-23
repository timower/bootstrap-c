// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Unknown primary expression error
// CHECK: : Unknow primary expression

func main() -> i32 {
  let x = if;  // 'if' is valid token but not valid primary expression
  return 0;
}