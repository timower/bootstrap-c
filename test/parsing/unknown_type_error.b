// RUN: not %bootstrap -sema %s 2>&1 | FileCheck %s
// Test unknown type parsing error with invalid token

// CHECK: Unknown type
func test(param: 123) -> i32 {
    return 0;
}

func main() -> i32 {
    return 0;
}