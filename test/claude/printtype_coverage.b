// RUN: %bootstrap -format %s | FileCheck %s

// Test case to improve coverage of printType function
// This test exercises various type kinds and edge cases in type formatting

extern func printf(format: i8*, ...) -> i32;

// Test function types with different return types and parameters
extern func complex_func(a: i32, b: const i8*, c: bool) -> i64;
extern func void_func() -> void;
extern func variadic_func(format: i8*, ...) -> i32;

// Test struct with various field types
struct ComplexStruct {
    // Test integer types with different sizes
    byte_field: i8;
    short_field: i16;
    int_field: i32;
    long_field: i64;
    
    // Test unsigned integer types
    ubyte_field: u8;
    ushort_field: u16;
    uint_field: u32;
    ulong_field: u64;
    
    // Test boolean
    bool_field: bool;
    
    // Test pointer types (including pointer to pointer)
    ptr_field: i32*;
    ptr_to_ptr: i32**;
    
    // Test array types
    array_field: i32[10];
    ptr_array: i32*[5];
    
    // Test const types
    const_int: const i32;
    const_ptr: const i8*;
};

// Test enum 
enum TestEnum {
    FIRST,
    SECOND,
    THIRD,
};

// Test union with various field types
union TestUnion {
    AsInt { value: i32; }
    AsPtr { ptr: i8*; }
    AsStruct { data: ComplexStruct; }
    AsArray { arr: i32[3]; }
};

func main() -> i32 {
    // Just declare variables to ensure types are processed during formatting
    let arr: i32[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    let ptr_arr: i32*[5] = {null, null, null, null, null};
    
    let s = ComplexStruct {
        byte_field = 1,
        short_field = 2,
        int_field = 3,
        long_field = 4,
        ubyte_field = 5,
        ushort_field = 6,
        uint_field = 7,
        ulong_field = 8,
        bool_field = true,
        ptr_field = null,
        ptr_to_ptr = null,
        array_field = arr,
        ptr_array = ptr_arr,
        const_int = 42,
        const_ptr = "test",
    };
    
    let e = TestEnum::FIRST;
    let u: TestUnion = TestUnion::AsInt { value = 123 };
    
    printf("Complex types test: %d %d\n", s.int_field, e as i32);
    return 0;
}

// CHECK: struct ComplexStruct {
// CHECK:     byte_field: i8;
// CHECK:     short_field: i16;
// CHECK:     int_field: i32;
// CHECK:     long_field: i64;
// CHECK:     ubyte_field: u8;
// CHECK:     ushort_field: u16;
// CHECK:     uint_field: u32;
// CHECK:     ulong_field: u64;
// CHECK:     bool_field: bool;
// CHECK:     ptr_field: i32*;
// CHECK:     ptr_to_ptr: i32**;
// CHECK:     array_field: i32[10];
// CHECK:     ptr_array: i32*[5];
// CHECK:     const_int: const i32;
// CHECK:     const_ptr: const i8*;
// CHECK: };