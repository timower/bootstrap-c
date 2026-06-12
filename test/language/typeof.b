// RUN: %brio %s | lli | FileCheck %s
// Test typeof operator in various contexts
// CHECK: nested typeof: x = 42, y = 100, z = 200
// CHECK: complex: a = 111, b = 222, c = 333
// CHECK: pointer typeof: a = 666, b = 777, c = 888
// CHECK: main: x = 42, y = 100, z = 200, w = 999
extern func printf(format: i8*, ...) -> i32;

func testNestedTypeof() {
  let x = 42;
  let y: typeof(x) = 100;
  let z: typeof(y) = 200;

  printf("nested typeof: x = %d, y = %d, z = %d\n", x, y, z);
}

func testComplexExpressions() {
  let arr = { 10, 20, 30, 40, 50};
  let ptr = &arr[0];

  let a: typeof(arr[0]) = 111;
  let b: typeof(*ptr) = 222;
  let c: typeof(arr[1] + arr[2]) = 333;

  printf("complex: a = %d, b = %d, c = %d\n", a, b, c);
}

func testPointerTypeof() {
  let value = 555;
  let ptr = &value;
  let ptrToPtr = &ptr;

  let a: typeof(value) = 666;
  let b: typeof(*ptr) = 777;
  let c: typeof(*(*ptrToPtr)) = 888;

  printf("pointer typeof: a = %d, b = %d, c = %d\n", a, b, c);
}

func main() -> i32 {
  // Basic local variable typeof
  let x = 0;
  let ptr: typeof(x)* = &x;
  *ptr = 42;
  let y: typeof(x) = 100;

  // Array element typeof
  let arr = { 1, 2, 3, 4, 5};
  let z: typeof(arr[0]) = 200;

  // Test nested typeof declarations
  testNestedTypeof();

  // Test complex expressions
  testComplexExpressions();

  // Test pointer dereferencing
  testPointerTypeof();

  // Test typeof with array indexing using variables
  let index = 2;
  let w: typeof(arr[index]) = 999;

  printf("main: x = %d, y = %d, z = %d, w = %d\n", x, y, z, w);

  return sizeof(typeof(index)) - 4;
}
