// RUN: %bootstrap %s | FileCheck %s
func foo() {

}

func bar() {
  // CHECK: call void () @foo
  // CHECK-NEXT: ret void
  return foo();
}
