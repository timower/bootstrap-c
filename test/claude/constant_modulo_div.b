// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli
// Test constant modulo and division to improve genConstant coverage

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    let arr: i32[4] = {15 / 3, 17 % 5, 20 * 2, 10 - 3};
    printf("Results: %d %d %d %d\n", arr[0], arr[1], arr[2], arr[3]);
    return 0;
}

// CHECK: define i32 @main()
// CHECK: alloca [4 x i32]
// CHECK: store i32 5
// CHECK: store i32 2
// CHECK: store i32 40
// CHECK: store i32 7
// CHECK: call i32 @printf(ptr {{.*}}, i32 {{.*}}