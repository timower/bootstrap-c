// RUN: %bootstrap %s | lli
// Test struct expressions and initialization

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