// RUN: %compile-and-run %s %t.ll
// RUN: grep alloca %t.ll | wc -l | grep 2
struct A {
  foo: i32;
}

func main() -> i32 {
  let x = A {
    foo = 0,
  };
  let y = x;
  x.foo = 42;
  return y.foo;
}
