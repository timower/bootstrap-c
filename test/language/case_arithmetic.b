// RUN: %bootstrap %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file=%t.ll
// Test arithmetic expressions in case statements

func main() -> i32 {
  let x = 10;
  // CHECK: switch i32
  switch (x) {
    case (5 + 5):
      return 0; // Success if arithmetic works
    case (2 + 3):
      return 2; // Different value (5)
    default:
      return 1; // Failure
  }
}