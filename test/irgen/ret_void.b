// RUN: %brio %s | opt -S | FileCheck %s
func foo() {

}

func bar() {
  // CHECK: call void @foo
  // CHECK: ret void
  return foo();
}
