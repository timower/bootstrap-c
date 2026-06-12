// RUN: %brio %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck --input-file=%t.ll %s

// CHECK-DAG: define i32 @foo
func foo() -> i32 {
  return 12;
}

struct Foo {
  bar: func*() -> i32;
  x: i32;
}


// CHECK-DAG: @globPtr = global ptr @foo
let globPtr = &foo;


// CHECK-DAG: define i32 @main
func main() -> i32 {
  // CHECK-DAG: store ptr @foo, ptr %alloc
  let x: func*() -> i32 = &foo;
  let ptr: func**() -> i32 = &x;

  let array: func*[3]() -> i32 = [ &foo, &foo, &foo ];

  let y = Foo {
    bar = array[1],
  };

  return (*ptr)() - y.bar() + y.x;
}
