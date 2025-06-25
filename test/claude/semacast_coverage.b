// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli

// Test case to improve coverage of semaCast function
// This test exercises different types of casts in semantic analysis

extern func printf(format: i8*, ...) -> i32;

// Test enum for enum to integer casts
enum TestEnum {
    VALUE1,
    VALUE2,
    VALUE3,
};

// Test struct for struct to union casts
struct TestStruct {
    x: i32;
    y: i32;
};

// Test union for union to pointer casts
union TestUnion {
    AsInt { value: i32; }
    AsStruct { data: TestStruct; }
};

func main() -> i32 {
    // Test integer to integer casts (different sizes)
    let i8_val: i8 = 10;
    let i16_val: i16 = i8_val as i16;    // i8 to i16 cast
    let i32_val: i32 = i16_val as i32;   // i16 to i32 cast
    let i64_val: i64 = i32_val as i64;   // i32 to i64 cast
    
    // Test unsigned to signed casts
    let u8_val: u8 = 200;
    let signed_val: i8 = u8_val as i8;   // u8 to i8 cast
    
    // Test integer literal casts (should be noop)
    let literal_cast = 42 as i32;        // integer literal cast
    
    // Test enum to integer casts
    let enum_val = TestEnum::VALUE2;
    let enum_as_int = enum_val as i32;   // enum to integer cast
    
    // Test integer to enum casts
    let int_as_enum = 1 as TestEnum;     // integer to enum cast
    
    // Test pointer casts
    let int_ptr: i32* = null;
    let void_ptr = int_ptr as void*;     // pointer to void* cast
    let back_ptr = void_ptr as i32*;     // void* back to typed pointer
    
    // Test struct/union related casts
    let test_struct = TestStruct { x = 10, y = 20 };
    let test_union: TestUnion = TestUnion::AsStruct { data = test_struct };
    
    // Test noop casts (same type)
    let same_type = i32_val as i32;      // same type cast (noop)
    
    printf("Cast tests: i16=%d, i32=%d, i64=%ld, enum=%d\n", 
           i16_val, i32_val, i64_val, enum_as_int);
    
    return 0;
}

// CHECK: define i32 @main()
// CHECK: sext i8 {{.*}} to i16
// CHECK: sext i16 {{.*}} to i32
// CHECK: sext i32 {{.*}} to i64
// CHECK: trunc i8 {{.*}} to i8
// CHECK: call i32 @printf(ptr {{.*}}