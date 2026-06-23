// RUN: %compile-and-run %s | FileCheck %s
// Test ternary conditional with struct types to cover genConditional isAggregate path
extern func printf(format: i8*, ...) -> i32;

struct Point {
  x: i32;
  y: i32;
}

func main() -> i32 {
  let p1 = Point {
    x = 10,
    y = 20,
  };
  let p2 = Point {
    x = 30,
    y = 40,
  };

  let a = true ? 1 : 2;

  // Test ternary with struct types (this should trigger isAggregate path)
  let result = true ? p1 : p2;

  // CHECK: Point: (10, 20)
  printf("Point: (%d, %d)\n", result.x, result.y);

  // Test with false condition
  let result2 = false ? p1 : p2;

  // CHECK: Point: (30, 40)
  printf("Point: (%d, %d)\n", result2.x, result2.y);

  return 0;
}
