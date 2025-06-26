// RUN: %bootstrap -format %s -o %t
// RUN: diff %t %s
//
// A struct
struct Foo {
  // comment 1
  x: i32;  // comment 2

  // comment 3
  // comment 3
  y: i64;  // comment 4
  // comment 5
};

union Bar {
  Void {}

  X {
    y: i32;
  }
};


// comment
func foo() {
  // comment
  let v = Foo {};  // comments
  // comment
}

func main() -> i32 {
  return 1 * (3 + 5) / 6;
}
