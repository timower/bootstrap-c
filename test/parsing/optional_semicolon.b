// RUN: %bootstrap %s | FileCheck %s
// Test that struct, union, and enum declarations work WITHOUT semicolons
// This verifies the optional semicolon feature by checking generated IR

struct Point {
    x: i32;
    y: i32;
}

union Result {
    Ok { value: i32; }
    Err { message: i8*; }
}

enum Status {
    ACTIVE,
    INACTIVE
}

func main() -> i32 {
    let p = Point { x = 10, y = 20 };
    let r: Result = Result::Ok { value = 42 };
    let s = Status::ACTIVE;
    return 0;
}

// CHECK-DAG: %struct.Point = type <{ i32, i32 }>
// CHECK-DAG: %union.Result = type <{ i32, [8 x i8] }>
// CHECK-DAG: %struct.Result.Ok = type <{ i32 }>
// CHECK: define i32 @main()
