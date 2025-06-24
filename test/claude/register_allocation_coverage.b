// RUN: %bootstrap %s | lli

// Test case to improve coverage of register allocation functions
// This test creates complex expressions to stress the register allocator

extern func printf(format: i8*, ...) -> i32;

func complex_expression(a: i32, b: i32, c: i32, d: i32) -> i32 {
    // Complex nested expression that should require many registers
    // This should trigger more register allocation paths
    let result = ((a + b) * (c - d)) + ((a * c) - (b + d)) + 
                 ((a - b) * (c + d)) - ((a / b) + (c % d));
    return result;
}

func main() -> i32 {
    // Test with multiple complex expressions to force register spilling
    let val1 = complex_expression(10, 5, 8, 3);
    let val2 = complex_expression(15, 7, 12, 4);
    let val3 = complex_expression(20, 9, 16, 6);
    
    // More complex operations
    let combined = (val1 + val2) * val3 - (val1 * val2) + val3;
    
    printf("Complex calculation result: %d\n", combined);
    
    return 0;
}