// Test to improve genConstant() function coverage by testing various constant scenarios
// RUN: %bootstrap %s | lli

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    // Integer constants of different sizes
    let i8_const: i8 = 42;
    let i16_const: i16 = 1234;
    let i32_const: i32 = 123456;
    let i64_const: i64 = 9876543210;
    
    // Unsigned integer constants
    let u8_const: u8 = 255;
    let u16_const: u16 = 65535;
    let u32_const: u32 = 4294967295;
    let u64_const: u64 = 18446744073709551615;
    
    // Boolean constants
    let bool_true: bool = true;
    let bool_false: bool = false;
    
    // Negative constants
    let neg_i8: i8 = -42;
    let neg_i16: i16 = -1234;
    let neg_i32: i32 = -123456;
    let neg_i64: i64 = -9876543210;
    
    // Zero constants
    let zero_i8: i8 = 0;
    let zero_i16: i16 = 0;
    let zero_i32: i32 = 0;
    let zero_i64: i64 = 0;
    let zero_u8: u8 = 0;
    let zero_u16: u16 = 0;
    let zero_u32: u32 = 0;
    let zero_u64: u64 = 0;
    
    // String constants
    let str_const: i8* = "Hello, World!";
    let empty_str: i8* = "";
    
    // Character constants
    let char_a: i8 = 'a';
    let char_newline: i8 = '\\n';
    let char_zero: i8 = '\\0';
    
    // Print to use the constants
    printf("i8: %d, i16: %d, i32: %d, i64: %ld\\n", 
           i8_const as i32, i16_const as i32, i32_const, i64_const);
    printf("u8: %u, u16: %u, u32: %u, u64: %lu\\n", 
           u8_const as u32, u16_const as u32, u32_const, u64_const);
    if (bool_true) {
        printf("bool true: 1, ");
    } else {
        printf("bool true: 0, ");
    }
    if (bool_false) {
        printf("bool false: 1\\n");
    } else {
        printf("bool false: 0\\n");
    }
    printf("negative: %d, %d, %d, %ld\\n", 
           neg_i8 as i32, neg_i16 as i32, neg_i32, neg_i64);
    printf("zeros: %d, %d, %d, %ld, %u, %u, %u, %lu\\n",
           zero_i8 as i32, zero_i16 as i32, zero_i32, zero_i64,
           zero_u8 as u32, zero_u16 as u32, zero_u32, zero_u64);
    printf("strings: '%s', '%s'\\n", str_const, empty_str);
    printf("chars: %c, code: %d, zero: %d\\n", 
           char_a, char_newline as i32, char_zero as i32);
    
    return 0;
}