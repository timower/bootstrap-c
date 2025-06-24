// RUN: %bootstrap %s | lli
// Test constant global pointer expressions to improve genConstant coverage

extern func printf(format: i8*, ...) -> i32;

let global_var: i32 = 42;
let global_ptr: i32* = &global_var;

func main() -> i32 {
    printf("Global var: %d, via pointer: %d\n", global_var, *global_ptr);
    return 0;
}