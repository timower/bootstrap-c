// RUN: %compile-and-run %s %t.ll
// RUN: FileCheck %s --input-file %t.ll
extern func printf(format: i8*, ...) -> i32;


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
    x: i32;
    y: i32;
  }
}

func main() -> i32 {
  let x: i64 = 42;
  let y: i32 = 100;
  let small: i8 = 5;

  // Test truncation cast (larger to smaller signed)
  // CHECK: trunc i64 {{.*}} to i32
  let truncated = x as i32;

  // Test sign extension cast (smaller to larger signed)
  // CHECK: sext i8 {{.*}} to i32
  let sign_extended = small as i32;

  // Test zero extension cast (unsigned)
  let usmall: u8 = 255;

  // CHECK: zext i8 {{.*}} to i32
  let zero_extended = usmall as u32;

  // Test enum to integer cast
  let color = Color::RED;
  let color_int = color as i32;

  // Test integer to enum cast
  let int_color = 1 as Color;

  // Test struct to union cast
  let point = Value::Point {
    x = 10,
    y = 20,
  };

  // CHECK: getelementptr inbounds %union.Value, ptr {{.*}}, i32 0, i32 0
  // CHECK: store i32 2, ptr {{.*}}
  // CHECK: getelementptr inbounds %union.Value, ptr {{.*}}, i32 0, i32 1
  // CHECK: call void (ptr, ptr, i32, i1) @llvm.memcpy.p0.p0.i32
  let point_union = point as Value;

  // Test union to struct pointer cast (type checking)
  let int_value = Value::Integer {
    value = 42,
  };

  // CHECK: getelementptr inbounds %struct.Value.Integer, ptr {{.*}}, i32 0, i32 0
  // CHECK: store i32 42, ptr {{.*}}
  let int_ptr = &int_value as Value::Integer*;

  // Test noop cast (same type)
  let same_type = y as i32;

  printf("Truncated: %d\n", truncated);
  printf("Sign extended: %d\n", sign_extended);
  printf("Zero extended: %u\n", zero_extended);
  printf("Color as int: %d\n", color_int);
  printf("Int as color: %d\n", int_color as i32);
  printf("Same type: %d\n", same_type);

  if (int_ptr != null) {
    printf("Union cast success: %d\n", int_ptr->value);
  } else {
    printf("Union cast failed\n");
  }

  return 0;
}
