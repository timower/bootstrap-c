// RUN: %bootstrap -format %s -o %t
// RUN: diff %t %s
// RUN: %bootstrap -format -i %t
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

func bar(str: i8*) {

}

let array = { "a", "b", "c"};

let list = {
  "a",
  "b",
  "c",
};

func long(
    a: Bar,
    b: Foo,
    x: i32
) -> Bar {
  // commentss
  return Bar::Void {};
}

enum Enum {
  Option1,
  Option2,
};

func main() -> i32 {
  let v = Bar::Void {} as Bar;

  v = Bar::X {
    y = (3 + 2)
        << 2,
  };

  bar("Test");

  let y = 2 + 2,
      ~3;

  bar(list[1]);

  long(
      v,
      Foo {},
      12);

  y++;

  y = 2 ? 1 : 0;
  y = sizeof(y) == 4
       ? 1
       : 0;

  let g = Enum::Option1;

  return 1 * (3 + 5) / 6;
}
