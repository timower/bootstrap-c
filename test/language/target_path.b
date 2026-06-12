// RUN: split-file %s %t
// RUN: %brio -target x86_64-unknown-linux-gnu %t/main.b | grep 'ret i32 0'
// RUN: %brio -target x86_64-apple-darwin %t/main.b | grep 'ret i32 1'
// RUN: %brio -target x86_64-w64-mingw32 %t/main.b | grep 'ret i32 2'
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

//--- foo.darwin.b
func foo() -> i32 {
  return 1;
}

//--- foo.windows.b
func foo() -> i32 {
  return 2;
}
