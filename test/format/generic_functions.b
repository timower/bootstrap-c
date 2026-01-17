// RUN: %bootstrap -format %s | cmp %s -
// RUN: echo 'func nogeneric[](){}' | %bootstrap -format - | grep 'func nogeneric()'
// Basic generic function
func swap[T](a: T*, b: T*) {
  let temp = *a;
  *a = *b;
  *b = temp;
}


func noGeneric(x: i32) -> i32 {
  return x;
}


// Multiple type parameters
func convert[T, U](input: T) -> U {
  return input as U;
}


// Generic function with multiple arguments
func max[T](a: T, b: T) -> T {
  return a > b ? a : b;
}


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
}


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
}
