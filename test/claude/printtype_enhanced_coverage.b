// RUN: %bootstrap -format %s | FileCheck %s

// Test case to improve coverage of printType function
// Focuses on missing branches: varargs functions, Tag types

extern func printf(format: i8*, ...) -> i32;

// Test varargs function (should hit varargs formatting branch)
extern func sprintf(buffer: i8*, format: i8*, ...) -> i32;

// Test function with multiple parameters and varargs
extern func complex_varargs(a: i32, b: const i8*, ...) -> void;

// Test enum with qualified names to trigger Tag type formatting
enum Status {
    ACTIVE,
    INACTIVE,
};

// Test using qualified enum names (should trigger Tag formatting)
func processStatus(status: Status) -> i32 {
    switch (status) {
        case Status::ACTIVE:
            return 1;
        case Status::INACTIVE:
            return 0;
    }
    return -1;
}

// Test complex function signature with all type variants
func complexFunction(
    basic: i32,
    ptr: i8*,
    arr: i32[5],
    const_param: const i64,
    enum_param: Status,
    ...
) -> Status {
    return Status::ACTIVE;
}

// Test struct with Tag types and complex nested types
struct ComplexType {
    status: Status;
    array_of_ptrs: i8*[10];
    const_array: const i32[3];
    ptr_to_func: void*;
};

func main() -> i32 {
    let status = Status::ACTIVE;
    let result = processStatus(status);
    
    printf("Status: %d\n", result);
    return 0;
}

// CHECK: extern func sprintf(buffer: i8*, format: i8*, ...) -> i32;
// CHECK: extern func complex_varargs(a: i32, b: const i8*, ...) -> void;
// CHECK: func complexFunction(
// CHECK:     basic: i32,
// CHECK:     ptr: i8*,
// CHECK:     arr: i32[5],
// CHECK:     const_param: const i64,
// CHECK:     enum_param: Status,
// CHECK:     ...
// CHECK: ) -> Status {
// CHECK:     return Status::ACTIVE;
// CHECK: }
// CHECK: struct ComplexType {
// CHECK:     status: Status;
// CHECK:     array_of_ptrs: i8*[10];
// CHECK:     const_array: const i32[3];
// CHECK:     ptr_to_func: void*;
// CHECK: };