// RUN: rm -rf %t
// RUN: split-file %s %t
///
// RUN: %brio -o %t.ll -debug  %t/main.b
//
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file=%t.ll
//
//
// RUN: cd %t && %brio main.b -o main.ll
// RUN: cd %t && lli main.ll
// RUN: cd %t && FileCheck %s --input-file=main.ll
//
//--- main.b
import dir.sub.foo;
import dir.lib;
import dir.bar;

func main() -> i32 {
  // CHECK: call i32 () @foo()
  return foo() - 22;
}


//--- dir/lib.b
import sub.foo;

func foo() -> i32 {
  return bar() + 11;
}


//--- dir/bar.b
func baz() -> i32 {
  return 11;
}


//--- dir/sub/foo.b
import bar;

func bar() -> i32 {
  return baz();
}
