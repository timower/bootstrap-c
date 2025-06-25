// Test to improve semaCast() function coverage by testing various casting scenarios
// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    // Integer type casting coverage
    let i8_val: i8 = 42;
    let i16_val: i16 = i8_val as i16;   // i8 to i16
    let i32_val: i32 = i16_val as i32;  // i16 to i32
    let i64_val: i64 = i32_val as i64;  // i32 to i64
    
    // Unsigned integer casting
    let u8_val: u8 = i8_val as u8;     // i8 to u8
    let u16_val: u16 = u8_val as u16;  // u8 to u16
    let u32_val: u32 = u16_val as u32; // u16 to u32
    let u64_val: u64 = u32_val as u64; // u32 to u64
    
    // Downcast scenarios
    let back_i32: i32 = i64_val as i32; // i64 to i32
    let back_i16: i16 = i32_val as i16; // i32 to i16
    let back_i8: i8 = i16_val as i8;    // i16 to i8
    
    // Mixed signed/unsigned casting
    let mixed1: u32 = i32_val as u32;   // i32 to u32
    let mixed2: i32 = u32_val as i32;   // u32 to i32
    let mixed3: u64 = i64_val as u64;   // i64 to u64
    let mixed4: i64 = u64_val as i64;   // u64 to i64
    
    // Pointer casting scenarios
    let ptr_i32: i32* = &i32_val;
    
    // Boolean value (no casting needed)
    let bool_val: bool = true;
    
    // Print results to verify
    printf("i8: %d, i16: %d, i32: %d, i64: %ld\\n", 
           i8_val as i32, i16_val as i32, i32_val, i64_val);
    printf("u8: %u, u16: %u, u32: %u, u64: %lu\\n", 
           u8_val as u32, u16_val as u32, u32_val, u64_val);
    printf("back casts: %d, %d, %d\\n", 
           back_i32, back_i16 as i32, back_i8 as i32);
    printf("mixed: %u, %d, %lu, %ld\\n", 
           mixed1, mixed2, mixed3, mixed4);
    if (bool_val) {
        printf("bool value: true\\n");
    } else {
        printf("bool value: false\\n");
    }
    printf("pointer value: %d\\n", *ptr_i32);
    
    return 0;
}

// CHECK: define i32 @main()
// CHECK: sext i8 {{.*}} to i16
// CHECK: sext i16 {{.*}} to i32
// CHECK: sext i32 {{.*}} to i64
// CHECK: zext i8 {{.*}} to i16
// CHECK: zext i16 {{.*}} to i32
// CHECK: zext i32 {{.*}} to i64
// CHECK: trunc i64 {{.*}} to i32
// CHECK: trunc i32 {{.*}} to i16
// CHECK: trunc i16 {{.*}} to i8