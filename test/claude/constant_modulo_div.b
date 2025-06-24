// RUN: %bootstrap %s | lli
// Test constant modulo and division to improve genConstant coverage

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    let arr: i32[4] = {15 / 3, 17 % 5, 20 * 2, 10 - 3};
    printf("Results: %d %d %d %d\n", arr[0], arr[1], arr[2], arr[3]);
    return 0;
}