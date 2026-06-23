// RUN: %compile-and-run %s %t.ll
// RUN: FileCheck %s --input-file %t.ll
extern func printf(format: i8*, ...) -> i32;

struct Point {
  x: i32;
  y: i32;
}

struct BigStruct {
  a: i64;
  b: i32;
  c: i8;
  d: Point;
}

enum Color {
  RED,
  GREEN,
  BLUE,
}

union Value {
  Integer {
    value: i32;
  }
  Long {
    value: i64;
  }
  Point {
    point: Point;
  }
}

func main() -> i32 {
  // CHECK: store i64 0, ptr %alloc{{.*}}
  let size_void = sizeof(void);

  // Test sizeof with primitive types
  // CHECK: store i64 1, ptr %alloc{{.*}}
  let size_i8 = sizeof(i8);

  // CHECK: store i64 2, ptr %alloc{{.*}}
  let size_i16 = sizeof(i16);

  // CHECK: store i64 4, ptr %alloc{{.*}}
  let size_i32 = sizeof(i32);

  // CHECK: store i64 8, ptr %alloc{{.*}}
  let size_i64 = sizeof(i64);

  // CHECK: store i64 1, ptr %alloc{{.*}}
  let size_u8 = sizeof(u8);

  // CHECK: store i64 2, ptr %alloc{{.*}}
  let size_u16 = sizeof(u16);

  // CHECK: store i64 4, ptr %alloc{{.*}}
  let size_u32 = sizeof(u32);

  // CHECK: store i64 8, ptr %alloc{{.*}}
  let size_u64 = sizeof(u64);

  // CHECK: store i64 1, ptr %alloc{{.*}}
  let size_bool = sizeof(bool);

  // CHECK: store i64 8, ptr %alloc{{.*}}
  let size_func = sizeof(func() -> i32);

  // CHECK: store i64 8, ptr %alloc{{.*}}
  let size_ptr = sizeof(i32*);

  // Test sizeof with struct
  // CHECK: store i64 8, ptr %alloc{{.*}}
  let size_point = sizeof(Point);

  // CHECK: store i64 21, ptr %alloc{{.*}}
  let size_big = sizeof(BigStruct);

  // Test sizeof with enum (should be i32)
  // CHECK: store i64 4, ptr %alloc{{.*}}
  let size_enum = sizeof(Color);

  // Test sizeof with union (should be size of largest variant + tag)
  // CHECK: store i64 12, ptr %alloc{{.*}}
  let size_union = sizeof(Value);

  // Test sizeof with array types
  // CHECK: store i64 20, ptr %alloc{{.*}}
  let size_array = sizeof(i32[5]);

  // CHECK: store i64 40, ptr %alloc{{.*}}
  let size_point_array = sizeof(Point[5]);

  // Test sizeof with variables
  let x: i64 = 42;
  let point = Point {
    x = 1,
    y = 2,
  };
  let numbers: i32[5] = [ 1, 2, 3, 4, 5 ];

  // CHECK: store i64 8, ptr %alloc{{.*}}
  let size_var_i64 = sizeof(typeof(x));

  // CHECK: store i64 8, ptr %alloc{{.*}}
  let size_var_point = sizeof(typeof(point));

  // CHECK: store i64 20, ptr %alloc{{.*}}
  let size_var_array = sizeof(typeof(numbers));

  printf("Primitive sizes:\n");
  printf("  i8: %d, i16: %d, i32: %d, i64: %d\n", size_i8, size_i16, size_i32, size_i64);
  printf("  u8: %d, u16: %d, u32: %d, u64: %d\n", size_u8, size_u16, size_u32, size_u64);
  printf("  bool: %d, ptr: %d\n", size_bool, size_ptr);

  printf("Struct sizes:\n");
  printf("  Point: %d, BigStruct: %d\n", size_point, size_big);

  printf("Other types:\n");
  printf("  Color enum: %d\n", size_enum);
  printf("  Value union: %d\n", size_union);

  printf("Array sizes:\n");
  printf("  i32[5]: %d, Point[5]: %d\n", size_array, size_point_array);

  printf("Variable sizes:\n");
  printf("  x (i64): %d, point: %d, numbers: %d\n", size_var_i64, size_var_point, size_var_array);

  return 0;
}
