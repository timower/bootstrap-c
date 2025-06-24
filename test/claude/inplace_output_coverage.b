// RUN: echo 'func main() -> i32 { return 0; }' | not %bootstrap -format -i 2>&1 | FileCheck %s --check-prefix=STDIN_ERROR
// RUN: not %bootstrap -format -i -o /tmp/test.out %s 2>&1 | FileCheck %s --check-prefix=BOTH_FLAGS_ERROR

// Test case to improve coverage of getOutOrInplaceFileName function
// This tests the error conditions in the function

// STDIN_ERROR: Cannot use -i with stdin input
// BOTH_FLAGS_ERROR: Cannot use both -i and -o

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    printf("This test exercises getOutOrInplaceFileName error paths\n");
    return 0;
}