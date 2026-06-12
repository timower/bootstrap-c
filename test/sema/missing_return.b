// RUN: not %brio -sema %s 2>&1 | FileCheck %s
// CHECK: 13:10: warning: Duplicate return
// CHECK-NOT: Duplicate return
// CHECK: 16:1: sema error: Not all paths return
// CHECK: 23:1: sema error: Not all paths return
// CHECK: 35:1: sema error: Not all paths return
func test2(x: i32) -> i32 {
  if (x == 0) {
    return 0;
  } else {
    return 1;
  }
  return 2;
}

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

enum Foo {
  A,
}

func test3(x: Foo) -> i32 {
  switch (x) {
    case Foo::A:
      break;
  }
}
