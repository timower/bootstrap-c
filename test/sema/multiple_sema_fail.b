// RUN: not %bootstrap -sema-lsp %s 2>&1 | FileCheck %s
//
// CHECK: 11:6: decl: [[FOO_ID:[0-9a-zA-Z]*]]: foo
// CHECK: 15:6: decl: [[MAIN_ID:[0-9a-zA-Z]*]]: main
// CHECK-DAG: 16:3: ref: [[FOO_ID]]: 3
// CHECK-DAG: 17:3: ref: [[FOO_ID]]: 3
// CHECK-DAG: 18:3: ref: [[FOO_ID]]: 3
//
// CHECK-DAG: 17:6: sema error: Function call arg length mismatch
// CHECK-DAG: 18:6: sema error: Function call arg length mismatch
func foo() {

}

func main() {
  foo();
  foo(12);
  foo(1, 2);
}
