// RUN: %bootstrap  %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file %t.ll
//
// Basic generic function declaration
extern func printf(s: i8*, ...) -> i32;
func swap[T](a: T*, b: T*) {
  let temp: T = *a;
  *a = *b;
  *b = temp;
}


union Union {
  A {}
  B {}
}

union Bunion {
  A {}
}

enum Foonum {
  A,
  B,
}

enum Barnum {
  B,
  C,
}

func getEnum[T]() -> T {
  return T::B;
}

func getUnion[T]() -> T {
  return T::A {};
}

func switchUnion[T](t: T) -> i32 {
  switch (t) {
    case T::A:
      return 1;
    case T::B as b:
      return 2;
    default:
      return 3;
  }
}


// Multiple type parameters
func convert[T, U](input: T) -> U {
  return input as U;
}


// Generic function with return type
func identity[T](value: T) -> T {
  return value;
}

func nested[U](v: U) -> U {
  return identity:[U](v);
}


func array[T]() -> T {
  let arr = [ identity:[T](1), identity:[T](2) ];
  return arr[0] + arr[1] + sizeof(T);
}

func getArray[T]() -> T[2] {
  return [ 1 as T, 2 as T ];
}


// Generic function using type parameters in complex ways
func process[T](data: [T], callback: func*(T) -> bool) -> bool {
  for (let i: iptr = 0; i < data.len; i++) {
    if (!callback(data[i])) {
      return false;
    }
  }
  while (false) {
    return false;
  }
  return true;
}


// Helper function for testing
func isPositive[T](n: T) -> bool {
  return n > 0;
}

func testCond[T](n: T) -> T {
  return isPositive:[T](n) ? n : -n;
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

  let unionVal = getUnion:[Union]();
  let unionVal2 = getUnion:[Bunion]();
  if (unionVal as Union::A* == null) {
    return 1;
  }
  let enumVal = getEnum:[Foonum]();
  if (enumVal != Foonum::B) {
    return 1;
  }

  let res = switchUnion:[Union](Union::A {});
  if (res != 1) {
    return 1;
  }

  let benumVal = getEnum:[Barnum]();

  let test = nested:[iptr](2);

  // Test identity function with different types
  // CHECK: call i32 (i32) @{{_?}}identity
  let int_val = identity:[i32](42);

  // CHECK: call i1 (i1) @{{_?}}identity
  let bool_val = identity:[bool](true);

  // Test convert with multiple types
  let char_val = convert:[i32, i8](41);

  let true_data = [ 1, 2, 3 ];
  let false_data = [ 1, -2, 3 ];
  if (!process:[i32](true_data[:], &isPositive:[i32])) {
    return 1;
  }

  if (process:[i32](false_data[:], &isPositive:[i32])) {
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

  let arrayTest = array:[i32]();
  if (arrayTest != 7) {
    return 1;
  }

  if (testCond:[i32](-5) != testCond:[i32](5)) {
    return 1;
  }

  let arr = getArray:[i32]();
  if (arr[0] != 1 || arr[1] != 2) {
    return 1;
  }

  return 0;
}
