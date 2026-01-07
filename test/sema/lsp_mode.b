// RUN: cat %s | not %bootstrap -stdin-filename test.b -sema-lsp 2>&1 | FileCheck %s
struct Bar {
  x: i32;
}

func foo() -> Bar {

}

func main() {
  foo();
  foo(12);
  foo(1, 2);
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
//
// CHECK: test.b:2:8: decl: [[BAR_ID:[0-9a-zA-Z]*]]: Bar
// CHECK: 3:3: decl: [[X_ID:[0-9a-zA-Z]*]]: x
// CHECK: 16:7: decl: [[UNION_ID:[0-9a-zA-Z]*]]: Union
// CHECK: 17:3: decl: [[A_ID:[0-9a-zA-Z]*]]: A
// CHECK: 18:3: decl: [[A_ID:[0-9a-zA-Z]*]]: B
// CHECK: 21:6: decl: [[A_ID:[0-9a-zA-Z]*]]: Enum
// CHECK: 22:3: decl: [[C_ID:[0-9a-zA-Z]*]]: C
// CHECK: 23:3: decl: [[D_ID:[0-9a-zA-Z]*]]: D
// CHECK: 6:6: decl: [[FOO_ID:[0-9a-zA-Z]*]]: foo
// CHECK: 10:6: decl: [[MAIN_ID:[0-9a-zA-Z]*]]: main
// CHECK-DAG: 6:15: ref: [[BAR_ID]]: 3
// CHECK-DAG: 11:3: ref: [[FOO_ID]]: 3
// CHECK-DAG: 12:3: ref: [[FOO_ID]]: 3
// CHECK-DAG: 13:3: ref: [[FOO_ID]]: 3
//
// CHECK-DAG: 12:6: sema error: Function call arg length mismatch
// CHECK-DAG: 13:6: sema error: Function call arg length mismatch
