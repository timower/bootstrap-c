// RUN: %bootstrap -format %s -o %t
// RUN: cmp %s %t
func foo() -> i32 {
  return 12;
}

func add(x: i32, y: i32) -> i32 {
  return x + y;
}

extern func varargs(x: i32, ...);
extern func simpleVarargs(...);

struct Foo {
  bar: func*() -> i32;
  x: i32;
}

func main() -> i32 {
  let x: func*() -> i32 = &foo;
  let ptr: func**() -> i32 = &x;

  let array: func*[3]() -> i32 = [ &foo, &foo, &foo ];

  let y = Foo {
    bar = array[1],
  };

  let v: func*(i32, ...) = &varargs;

  let w: func*(...) = &simpleVarargs;

  let u: func*(i32, i32) -> i32 = &add;

  let a = null as func*() -> i32;

  return (*ptr)() - y.bar() + y.x;
}
