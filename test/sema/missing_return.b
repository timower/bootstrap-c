// RUN: not %bootstrap -sema %s 2>&1 | FileCheck %s
// CHECK: 5:1: sema error: Not all paths return
// CHECK: 12:1: sema error: Not all paths return
// CHECK: 26:10: warning: Duplicate return
func foo(x: i32) -> i32 {
  switch (x) {
    case 1:
      return 0;
  }
}

func test(x: i32) -> i32 {
  if (x == 0) {
    return 0;
  } else {

  }
}

func test2(x: i32) -> i32 {
  if (x == 0) {
    return 0;
  } else {
    return 1;
  }
  return 2;
}
