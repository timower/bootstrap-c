// RUN: %bootstrap -format %s -o %t
// RUN: diff %t %s
// RUN: %bootstrap -format -i %t
// RUN: diff %t %s
// RUN: cat %s | %bootstrap -format -o %t
// RUN: diff %t %s
//
extern func printf(format: i8*, ...) -> i32;


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
func foo(x: Foo) {
  // comment
  let v = Foo {};  // comments
  let z = v as struct Foo;
  // comment
}

func bar(str: const i8*, b: Bar::Void) {
  let y: void = 0;
  let x: i8[5] = { 0, 1, 2 };
  let x: u32[] = { 0 };
}

let array = { "a", "b", "c" };

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
  let b: bool = sizeof(y) == 4;
  y = b
       ? 1
       : 0;

  let g: enum Enum = Enum::Option1;

  // Test if statement with compound blocks
  if (y > 0) {
    printf("Positive number\n");
  }

  // Test if-else statement
  if (y == 0) {
    printf("Zero\n");
  } else {
    printf("Non-zero\n");
  }

  // Test let expression in if condition
  if (let ptr = &y) {
    printf("Got pointer: %p\n", ptr);
  }

  // Test while loop
  let i = 0;
  while (i < 3) {
    printf("Loop iteration: %d\n", i);
    i++;
  }

  // Test for loop with all parts
  for (let j = 0; j < 5; j++) {
    printf("For loop: %d\n", j);

    // Test break statement
    if (j == 3) {
      break;
    }
  }

  // Test switch statement with enum
  switch (g) {
    case Enum::Option1:
      printf("Option 1 selected\n");
      break;
    case Enum::Option2:
      printf("Option 2 selected\n");
      break;
  }

  // Test switch with multiple case values
  let number = 2;
  switch (number) {
    case 1, 2, 3:
      printf("Small number\n");
      break;
    default:
      printf("Other number\n");
      break;
  }

  // Test switch with union pattern matching
  switch (v as union Bar) {
    case Bar::Void:
      printf("Void variant\n");
      break;
    case Bar::X as x:
      printf("X variant with y=%d\n", x.y);
      break;
  }

  // Test compound statement with nested scope
{
    let local_var = 42;
{
      let nested_var = local_var * 2;
      printf("Nested: %d\n", nested_var);
    }
      // nested_var not accessible here
}

  // Test expression statements
  printf("Expression statement\n");
  y += 10;
  ;  // Empty statement

  return 1 * (3 + 5) / 6;
}
