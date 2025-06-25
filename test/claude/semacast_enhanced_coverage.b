// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli

// Test case to improve coverage of semaCast function
// Focuses on missing branches: array pointer casts, pointer void casts

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    // Test array pointer decay casts (array to element pointer)
    // This should hit the array pointer cast path in semaCast
    let arr: i32[5] = {1, 2, 3, 4, 5};
    let arr_ptr = &arr;                    // pointer to array
    let elem_ptr = arr_ptr as i32*;       // array pointer cast to element pointer
    
    // Test void pointer casts (both directions)
    // This should hit the void pointer cast paths
    let int_ptr: i32* = &arr[0];
    let void_ptr = int_ptr as void*;       // typed pointer to void*
    let back_ptr = void_ptr as i32*;       // void* back to typed pointer
    
    // Test multiple levels of pointer casts
    let char_ptr = void_ptr as i8*;        // void* to different typed pointer
    
    // Test different sized integer casts to exercise semaIntCast paths
    let large_val: i64 = 0x123456789ABCDEF;
    let int_val = large_val as i32;        // i64 to i32 (truncation)
    let short_val = int_val as i16;        // i32 to i16 (truncation)
    let byte_val = short_val as i8;        // i16 to i8 (truncation)
    
    // Test unsigned to signed casts
    let unsigned_val: u32 = 0x80000000;
    let signed_val = unsigned_val as i32;  // u32 to i32 (reinterpret)
    
    // Test using the casted values
    printf("Array element via cast: %d\n", *elem_ptr);
    printf("Back pointer value: %d\n", *back_ptr);
    printf("Integer cast result: %d\n", int_val);
    printf("Signed cast result: %d\n", signed_val);
    
    return 0;
}

// CHECK: define i32 @main()
// CHECK: alloca [5 x i32]
// CHECK: alloca ptr
// CHECK: bitcast ptr
// CHECK: call i32 @printf(ptr {{.*}}, i32 %{{[0-9]+}})
// CHECK: trunc i64 {{.*}} to i32
// CHECK: trunc i32 {{.*}} to i16
// CHECK: trunc i16 {{.*}} to i8