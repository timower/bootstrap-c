// RUN: %compile-and-run %s %t.ll
// RUN: FileCheck %s --input-file=%t.ll
func main() -> i32 {
  // CHECK: call i32 (i32) @foo(i32 12)
  return foo(12);
}

func foo(x: i32) -> i32 {
  if (x == 0) {
    return 0;
  }
  return bar(x - 1);
}

func bar(y: i32) -> i32 {
  if (y == 0) {
    return 1;
  }
  return foo(y - 1);
}
