// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Invalid extern declaration
// CHECK: : Expected func or let

extern struct BadStruct {  // extern can only be used with func or let
  x: i32;
};

func main() -> i32 {
  return 0;
}