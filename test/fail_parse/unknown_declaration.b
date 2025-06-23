// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Unknown declaration
// CHECK: : Unknown declaration

class MyClass {  // 'class' is not a valid declaration in Bootstrap
  x: i32;
}

func main() -> i32 {
  return 0;
}