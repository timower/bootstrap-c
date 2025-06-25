// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli
// Test enums

enum Color {
  RED,
  GREEN,
  BLUE,
};

enum Status {
  PENDING,
  RUNNING,
  COMPLETED,
  FAILED,
};

enum Direction {
  NORTH,
  SOUTH,
  EAST,
  WEST,
};

func getColorValue(c: Color) -> i32 {
  switch (c) {
    case Color::RED:
      return 1;
    case Color::GREEN:
      return 2;
    case Color::BLUE:
      return 3;
  }
  return 0;
}

func checkStatus(s: Status) -> bool {
  switch (s) {
    case Status::PENDING, Status::RUNNING:
      return false;
    case Status::COMPLETED:
      return true;
    case Status::FAILED:
      return false;
  }
  return false;
}

func main() -> i32 {
  let red = Color::RED;
  let green = Color::GREEN;
  let blue = Color::BLUE;
  
  let pending = Status::PENDING;
  let completed = Status::COMPLETED;
  
  let north = Direction::NORTH;
  
  let redValue = getColorValue(red);
  let greenValue = getColorValue(green);
  let blueValue = getColorValue(blue);
  
  let isComplete = checkStatus(completed);
  let isPending = checkStatus(pending);
  
  return 0;
}

// CHECK: define i32 @main()
// CHECK: define i32 @getColorValue(i32 %0)
// CHECK: define i1 @checkStatus(i32 %0)
// CHECK: switch i32 %{{[0-9]+}}, label %{{[0-9]+}} [
// CHECK: switch i32 %{{[0-9]+}}, label %{{[0-9]+}} [
// CHECK: call i32 @getColorValue(i32 0)
// CHECK: call i32 @getColorValue(i32 1)
// CHECK: call i32 @getColorValue(i32 2)