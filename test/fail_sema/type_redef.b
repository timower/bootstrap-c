// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Type redefinition error
// CHECK: sema error: : Type redef

struct Point {
  x: i32;
  y: i32;
};

struct Point {  // Redefinition of Point
  a: i32;
  b: i32;
};

func main() -> i32 {
  return 0;
}