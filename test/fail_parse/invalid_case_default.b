// RUN: not %bootstrap %s 2>&1 | FileCheck %s
// Test: Invalid case/default in switch
// CHECK: : Expected case or default

func main() -> i32 {
  let x = 1;
  switch (x) {
    invalid_label:  // not 'case' or 'default'
      return 1;
  }
  return 0;
}