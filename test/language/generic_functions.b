// RUN: %bootstrap  %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file %t.ll
//
// Basic generic function declaration
func swap[T](a: T*, b: T*) {
  let temp: T = *a;
  *a = *b;
  *b = temp;
}


union Union {
  A {}
}


// Multiple type parameters
func convert[T, U](input: T) -> U {
  return input as U;
}


// Generic function with return type
func identity[T](value: T) -> T {
  return value;
}


// Generic function using type parameters in complex ways
func process[T](data: T*, size: i32, callback: func*(T) -> bool) -> bool {
  for (let i = 0; i < size; i++) {
    if (!callback(*(data + i))) {
      return false;
    }
  }
  return true;
}


// Helper function for testing
func isPositive[T](n: T) -> bool {
  return n > 0;
}


struct Foo {
  foo: i32;
}

func getFoo[T](t: T*) -> i32 {
  return t->foo;
}

func addFoo[T](f: Foo*, t: T) -> i32 {
  return f->foo + t;
}


// CHECK-LABEL: define i32 @{{_?}}main
func main() -> i32 {
  // Test simple instantiation first
  let x = 42;
  let y = 84;
  swap:[i32](&x, &y);

  let a = true;
  let b = false;
  swap:[bool](&a, &b);

  // Test identity function with different types
  // CHECK: call i32 (i32) @{{_?}}identity
  let int_val = identity:[i32](42);

  // CHECK: call i1 (i1) @{{_?}}identity
  let bool_val = identity:[bool](true);

  // Test convert with multiple types
  let char_val = convert:[i32, i8](41);

  let true_data = { 1, 2, 3 };
  let false_data = { 1, -2, 3 };
  if (!process:[i32](&true_data, 3, &isPositive:[i32])) {
    return 1;
  }

  if (process:[i32](&false_data, 3, &isPositive:[i32])) {
    return 1;
  }

  let foo = Foo {
    foo = 0,
  };
  let u = getFoo:[Foo](&foo);
  let v = addFoo:[i32](&foo, 5);

  let bar: Union = Union::A {};
  let ptr = &bar;
  let aptr = convert:[Union*, Union::A*](&bar);
  return 0;
}
