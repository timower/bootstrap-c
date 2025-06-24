// RUN: split-file %s %t
// RUN: %bootstrap -target posix %t/main.b | opt -p verify | lli
//--- main.b
import foo;

func main() -> i32 {
  return foo();
}


//--- foo.posix.b
func foo() -> i32 {
  return 0;
}
