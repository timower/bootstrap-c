// RUN: %bootstrap -format %s | FileCheck %s
// Basic generic function
func swap[T](a: T*, b: T*) {
  let temp = *a;
  *a = *b;
  *b = temp;
}

// CHECK: func noGeneric(x: i32) -> i32 {
func noGeneric[](x: i32) -> i32 {
  return x;
}


// CHECK: func swap[T](a: T*, b: T*) {
// CHECK-NEXT:   let temp = *a;
// CHECK-NEXT:   *a = *b;
// CHECK-NEXT:   *b = temp;
// CHECK-NEXT: }
// Multiple type parameters
func convert[T, U](input: T) -> U {
  return input as U;
}


// CHECK: func convert[T, U](input: T) -> U {
// CHECK-NEXT:   return input as U;
// CHECK-NEXT: }
// Generic function with multiple arguments
func max[T](a: T, b: T) -> T {
  return a > b ? a : b;
}


// CHECK: func max[T](a: T, b: T) -> T {
// CHECK-NEXT:   return a > b ? a : b;
// CHECK-NEXT: }

// Generic function with complex types
func process[T](
    data: T*,
    size: i32,
    callback: func(T) -> bool
) -> bool {
  for (let i = 0; i < size; i++) {
    if (!callback(data[i])) {
      return false;
    }
  }
  return true;
} // CHECK: func process[T](


// CHECK-NEXT:     data: T*,
// CHECK-NEXT:     size: i32,
// CHECK-NEXT:     callback: func(T) -> bool
// CHECK-NEXT: ) -> bool {
// CHECK-NEXT:   for (let i = 0; i < size; i++) {
// CHECK-NEXT:     if (!callback(data[i])) {
// CHECK-NEXT:       return false;
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT:   return true;
// CHECK-NEXT: }
// Generic function instantiations
func main() -> i32 {
  let x = 5;
  let y = 10;

  // Basic instantiation
  swap:[i32](&x, &y);

  // Multiple type arguments
  let result = convert:[i32, f64](42);

  // Nested generic instantiation
  let ptr = &x;
  swap:[i32*](&ptr, &ptr);

  return 0;
} // CHECK: func main() -> i32 {
// CHECK-NEXT:   let x = 5;
// CHECK-NEXT:   let y = 10;
// CHECK:   swap:[i32](&x, &y);
// CHECK:   let result = convert:[i32, f64](42);
// CHECK:   let ptr = &x;
// CHECK-NEXT:   swap:[i32*](&ptr, &ptr);
// CHECK:   return 0;
// CHECK-NEXT: }
