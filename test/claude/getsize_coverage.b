// Test case to improve coverage of getSize function
// This test exercises different type size calculations

// RUN: %bootstrap %s | lli

extern func printf(format: i8*, ...) -> i32;

// Test struct with various field types to trigger struct size calculation
struct SimpleStruct {
    a: i8;      // 1 byte
    b: i16;     // 2 bytes  
    c: i32;     // 4 bytes
    d: i64;     // 8 bytes
};

// Test empty struct (should return size 1)
struct EmptyStruct {
};

// Test nested struct
struct NestedStruct {
    simple: SimpleStruct;
    value: i32;
};

// Test union with different sized fields to trigger union size calculation
union TestUnion {
    Small { byte_val: i8; }
    Medium { int_val: i32; }  
    Large { long_val: i64; }
    Struct { struct_val: SimpleStruct; }
};

// Test enum (should be 4 bytes)
enum TestEnum {
    VALUE1,
    VALUE2,
};

func main() -> i32 {
    // Test different types to trigger getSize function calls
    
    // Test void type (should be 0 bytes)
    // This is harder to test directly, but might be triggered in type analysis
    
    // Test bool type (should be 1 byte)
    let bool_var: bool = true;
    
    // Test integer types of different sizes
    let i8_var: i8 = 1;
    let i16_var: i16 = 2;
    let i32_var: i32 = 3;
    let i64_var: i64 = 4;
    
    let u8_var: u8 = 5;
    let u16_var: u16 = 6;
    let u32_var: u32 = 7;
    let u64_var: u64 = 8;
    
    // Test pointer types (should be 8 bytes on 64-bit)
    let int_ptr: i32* = null;
    let struct_ptr: SimpleStruct* = null;
    
    // Function pointers may not be supported as variables
    
    // Test array types with known sizes
    let int_array: i32[5] = {1, 2, 3, 4, 5};
    
    // Test enum
    let enum_var: TestEnum = TestEnum::VALUE1;
    
    // Test struct instances
    let simple_struct = SimpleStruct { a = 1, b = 2, c = 3, d = 4 };
    let empty_struct = EmptyStruct {};
    let nested_struct = NestedStruct { 
        simple = simple_struct, 
        value = 42 
    };
    
    // Test union instances
    let union_small: TestUnion = TestUnion::Small { byte_val = 1 };
    let union_large: TestUnion = TestUnion::Large { long_val = 123 };
    
    printf("All type size tests completed: %d %d %d\n", 
           simple_struct.c, enum_var as i32, nested_struct.value);
    
    return 0;
}