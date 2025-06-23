// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Switch on invalid type
// CHECK: sema error: Switch expr must be integer, enum or union

func main() -> i32 {
  let x: bool = true;
  switch (x) {  // Switch on bool not allowed
    case true:
      return 1;
  }
  return 0;
}