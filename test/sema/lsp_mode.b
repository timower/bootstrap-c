struct Bar {
  x: i32;
}

func foo(y: Bar) -> i32 {
  return y.x;
}

func main() {
  foo();
  foo(Bar {});
  foo(Bar {}, Bar {});
}

union Union {
  A {}
  B {}
}

enum Enum {
  C,
  D,
}

let x: Enum = Enum::C;

let y: Union = Union::A {}; //

let z: Union = Union::Z {}; //
// RUN: cat %s | not %brio -stdin-filename test.b -sema-lsp 2>&1 \
// RUN:  | FileCheck %s
// RUN: not %brio %s -sema 2>&1 \
// RUN:  | FileCheck --check-prefix=NOLSP %s
//
// RUN: not %brio %s -sema-lsp 2>&1 \
// RUN:  | FileCheck --check-prefix=CHECK2 %s
//
// NOLSP-NOT: decl
// NOLSP-NOT: ref
//
// CHECK2-NOT: ref: (nil): 1
// CHECK2-NOT: ref: 0x0: 1
//
// CHECK: test.b:1:8: decl: [[BAR_ID:[0-9a-zA-Z]*]]: Bar
// CHECK: 2:3: decl: [[X_ID:[0-9a-zA-Z]*]]: x
// CHECK: 15:7: decl: [[UNION_ID:[0-9a-zA-Z]*]]: Union
// CHECK: 16:3: decl: [[A_ID:[0-9a-zA-Z]*]]: A
// CHECK: 17:3: decl: [[B_ID:[0-9a-zA-Z]*]]: B
// CHECK: 20:6: decl: [[Enum_ID:[0-9a-zA-Z]*]]: Enum
// CHECK: 21:3: decl: [[C_ID:[0-9a-zA-Z]*]]: C
// CHECK: 22:3: decl: [[D_ID:[0-9a-zA-Z]*]]: D
// CHECK: 5:6: decl: [[FOO_ID:[0-9a-zA-Z]*]]: foo
// CHECK: 9:6: decl: [[MAIN_ID:[0-9a-zA-Z]*]]: main
//
// CHECK-DAG: 5:13: ref: [[BAR_ID]]: 3
// CHECK-DAG: 6:12: ref: [[X_ID]]: 1
// CHECK-DAG: 10:3: ref: [[FOO_ID]]: 3
// CHECK-DAG: 11:3: ref: [[FOO_ID]]: 3
// CHECK-DAG: 12:3: ref: [[FOO_ID]]: 3
// CHECK-DAG: 27:23: ref: [[A_ID]]: 1
//
// CHECK-DAG: 10:6: sema error: Function call arg length mismatch
// CHECK-DAG: 12:6: sema error: Function call arg length mismatch
// CHECK-DAG: 29:25: sema error: Expected struct type for struct init expression
//
