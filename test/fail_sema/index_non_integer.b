// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Index with non-integer
// CHECK: sema error: Can't index with non integer
func test_func(arr: i32[]) -> i32 {
  let idx: bool = true;
  let y = arr[idx];  // Array syntax causes parser error
  return 0;
}

func main() -> i32 {
  return 0;
}

