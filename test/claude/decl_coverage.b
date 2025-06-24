// RUN: %bootstrap %s | lli
// Test case to improve coverage of isDecl function
// The isDecl function is used in sizeof parsing to determine if the
// argument is a type (vs an expression). This tests those code paths.
extern func printf(format: i8*, ...) -> i32;


// Test struct for sizeof type testing
struct TestStruct {
  x: i32;
  y: i32;
};


// Test enum for sizeof type testing
enum TestEnum {
  VALUE1,
  VALUE2,
};


// Test union for sizeof type testing
union TestUnion {
  AsInt {
    value: i32;
  }
  AsChar {
    value: i8;
  }
};

func main() -> i32 {
  // Test sizeof with different types - this exercises isDecl function
  // Each sizeof(type) call triggers isDecl to determine it's a type, not an expression
  // Test sizeof with struct type (should trigger isDecl with STRUCT token)
  let struct_size = sizeof(struct TestStruct);

  // Test sizeof with enum type (should trigger isDecl with ENUM token)
  let enum_size = sizeof(enum TestEnum);

  // Test sizeof with union type (should trigger isDecl with UNION token)
  let union_size = sizeof(union TestUnion);

  // Test sizeof with variables (expression path, not type path)
  let var = 42;
  let var_size = sizeof(var);

  printf(
      "Sizes: struct=%d enum=%d union=%d var=%d\n",
      struct_size,
      enum_size,
      union_size,
      var_size);

  return 0;
}

