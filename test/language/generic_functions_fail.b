// RUN: not %bootstrap -sema %s 2>&1 | FileCheck %s
// CHECK: Expected pointer type for *
func deref[T](ptr: T) -> T {
  return *ptr;
}


// Generic function with specific constraints
func add[T](a: T) -> T {
  return a.foo;  // This will fail for non-numeric types
}

func main() -> i32 {
  let test = add:[bool];

  // This should fail - trying to dereference a non-pointer
  let b = add:[bool](true);

  let x = 42;
  let result = deref:[i32](x);  // Error: expected T*, got i32

  return 0;
}
