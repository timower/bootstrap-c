// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli
// Test arithmetic expressions in case statements - should return 0 if arithmetic works

// CHECK: alloca i32
// CHECK: store i32 10
// CHECK: switch i32
// CHECK: i32 10, label
// CHECK: ret i32 0

func main() -> i32 {
  let x = 10;
  switch (x) {
    case (5 + 5):
      return 0; // Success if arithmetic works
    case (2 + 3):
      return 2; // Different value (5)
    default:
      return 1; // Failure
  }
}