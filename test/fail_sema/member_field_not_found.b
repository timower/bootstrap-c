// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Struct field not found
// CHECK: sema error:  Cannot find field

struct Point {
  x: i32;
  y: i32;
};

func main() -> i32 {
  let p = Point { x = 1, y = 2 };
  let z = p.z;  // Field 'z' doesn't exist
  return 0;
}