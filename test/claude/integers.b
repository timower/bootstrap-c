// RUN: %bootstrap %s | lli
// Test integer types and basic operations

func main() -> i32 {
    let a: i32 = 10;
    let b: i32 = 5;
    let add = a + b;
    let sub = a - b;
    let mul = a * b;
    let div = a / b;
    
    let i8_val: i8 = 100;
    let i16_val: i16 = 30000;
    let i32_val: i32 = 123456;
    let i64_val: i64 = 987654321;
    
    let u8_val: u8 = 200;
    let u16_val: u16 = 50000;
    let u32_val: u32 = 4000000000;
    let u64_val: u64 = 10000000000;
    
    return 0;
}