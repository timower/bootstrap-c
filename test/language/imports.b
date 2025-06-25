// RUN: split-file %s %t
//
// RUN: %bootstrap %t/main.b | FileCheck %s
// RUN: %bootstrap %t/main.b | lli
// RUN: cd %t && %bootstrap main.b | lli
//
//--- main.b
import dir.sub.foo;
import dir.lib;

func main() -> i32 {
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

// CHECK: define i32 @main()
// CHECK: define i32 @foo()
// CHECK: define i32 @bar()
// CHECK: define i32 @baz()
// CHECK: call i32 @foo()
// CHECK: call i32 @bar()
// CHECK: call i32 @baz()
