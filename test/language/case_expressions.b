// RUN: %compile-and-run %s %t.ll
// RUN: FileCheck %s --input-file=%t.ll
// Test various valid case expression types

enum Color {
  RED,
  GREEN,
  BLUE,
};

union Shape {
  Circle { radius: i32; }
  Square { size: i32; }
  Triangle {}
};

func main() -> i32 {
  // Test arithmetic expressions in case statements
  let x = 10;
  // CHECK: switch i32
  switch (x) {
    case (5 + 5):
      break;
    case (20 - 5):
      return 1; // Should not reach here
    default:
      return 2;
  }

  // Test enum scope expressions
  let color = Color::RED;
  switch (color) {
    case Color::RED:
      break;
    case Color::GREEN, Color::BLUE:
      return 3;
  }

  // Test union scope expressions
  let shape: Shape = Shape::Triangle {};
  switch (shape) {
    case Shape::Circle as c:
      return 4;
    case Shape::Square as s:
      return 5;
    case Shape::Triangle:
      break;
  }

  return 0; // Success
}
