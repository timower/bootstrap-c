// RUN: split-file %s %t
// RUN: %bootstrap -target posix %t/main.b -o %t.ll
// RUN: opt -p verify %t.ll | lli
// RUN: FileCheck %s --input-file=%t.ll
//--- main.b
import foo;

func main() -> i32 {
  // CHECK: call i32 () @foo()
  return foo();
}


//--- foo.posix.b
func foo() -> i32 {
  return 0;
}
