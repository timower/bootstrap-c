// RUN: %bootstrap %s -o %t.ll
// RUN: FileCheck %s < %t.ll
// RUN: lli %t.ll
// Test struct expressions and initialization

// CHECK: %struct.Rectangle = type
// CHECK: %struct.Point = type
// CHECK: alloca %struct.Rectangle
// CHECK: alloca %struct.Point
// CHECK: getelementptr inbounds %struct.Point

struct Point {
  x: i32;
  y: i32;
};

struct Rectangle {
  top_left: Point;
  bottom_right: Point;
};

func main() -> i32 {
  let p = Point{
    x = 10,
    y = 20,
  };
  
  let rect = Rectangle {
    top_left = Point { x = 0, y = 0 },
    bottom_right = Point { x = 10, y = 10 },
  };
  
  let sum = p.x + p.y + rect.bottom_right.x;
  
  return 0;
}