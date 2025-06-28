// RUN: %bootstrap %s | lli | FileCheck %s
// Test comprehensive binary expressions functionality

// CHECK: === Arithmetic Operations ===
// CHECK-NEXT: 15 + 4 = 19
// CHECK-NEXT: 15 - 4 = 11
// CHECK-NEXT: 15 * 4 = 60
// CHECK-NEXT: 15 / 4 = 3
// CHECK-NEXT: 15 % 4 = 3
// CHECK: === Comparison Operations ===
// CHECK-NEXT: 10 == 20: false
// CHECK-NEXT: 10 == 10: true
// CHECK-NEXT: 10 != 20: true
// CHECK-NEXT: 10 != 10: false
// CHECK-NEXT: 10 < 20: true
// CHECK-NEXT: 20 < 10: false
// CHECK-NEXT: 10 <= 20: true
// CHECK-NEXT: 10 <= 10: true
// CHECK-NEXT: 20 > 10: true
// CHECK-NEXT: 10 > 20: false
// CHECK-NEXT: 20 >= 10: true
// CHECK-NEXT: 10 >= 10: true
// CHECK: === Logical Operations ===
// CHECK-NEXT: true && true: true
// CHECK-NEXT: true && false: false
// CHECK-NEXT: false && true: false
// CHECK-NEXT: false && false: false
// CHECK-NEXT: true || true: true
// CHECK-NEXT: true || false: true
// CHECK-NEXT: false || true: true
// CHECK-NEXT: false || false: false
// CHECK: === Bitwise Operations ===
// CHECK-NEXT: 12 & 10 = 8
// CHECK-NEXT: 12 | 10 = 14
// CHECK-NEXT: 12 ^ 10 = 6
// CHECK-NEXT: 12 << 2 = 48
// CHECK-NEXT: 12 >> 2 = 3
// CHECK: === Mixed Type Operations ===
// CHECK-NEXT: i8(5) + i32(100000) = 100005
// CHECK-NEXT: i16(300) * i8(5) = 1500
// CHECK-NEXT: u8(200) + u32(50000) = 50200
// CHECK: === Short-Circuit Evaluation ===
// CHECK-NEXT: Counter after false && expr: 0
// CHECK-NEXT: AND condition met
// CHECK-NEXT: Counter after true && expr: 1
// CHECK-NEXT: OR condition met
// CHECK-NEXT: Counter after true || expr: 1
// CHECK-NEXT: OR fallback condition met
// CHECK-NEXT: Counter after false || expr: 2
// CHECK: === Operator Precedence ===
// CHECK-NEXT: 2 + 3 * 4 = 14
// CHECK-NEXT: 20 - 4 / 2 = 18
// CHECK-NEXT: 5 + 3 > 4 * 2: false
// CHECK-NEXT: 5 > 3 && 2 < 4: true
// CHECK-NEXT: 4 + 2 & 3 = 2
// CHECK: === Operator Associativity ===
// CHECK-NEXT: 20 - 5 - 3 = 12
// CHECK-NEXT: 24 / 4 / 2 = 3
// CHECK-NEXT: true && false || true: true
// CHECK: === Pointer Arithmetic ===
// CHECK-NEXT: *(ptr + 2) = 30
// CHECK-NEXT: ptr_end - ptr_start = 2
// CHECK: All binary expression tests completed

extern func printf(format: i8*, ...) -> i32;

func printBool(value: bool, name: i8*) {
    if (value) {
        printf("%s: true\n", name);
    } else {
        printf("%s: false\n", name);
    }
}

// Test arithmetic operations
func testArithmetic() {
    printf("=== Arithmetic Operations ===\n");
    
    let a = 15;
    let b = 4;
    
    // Addition
    let add_result = a + b;
    printf("%d + %d = %d\n", a, b, add_result);
    
    // Subtraction  
    let sub_result = a - b;
    printf("%d - %d = %d\n", a, b, sub_result);
    
    // Multiplication
    let mul_result = a * b;
    printf("%d * %d = %d\n", a, b, mul_result);
    
    // Division
    let div_result = a / b;
    printf("%d / %d = %d\n", a, b, div_result);
    
    // Modulo
    let mod_result = a % b;
    printf("%d %% %d = %d\n", a, b, mod_result);
}

// Test comparison operations
func testComparison() {
    printf("=== Comparison Operations ===\n");
    
    let x = 10;
    let y = 20;
    let z = 10;
    
    // Equality
    printf("%d == %d: %s\n", x, y, (x == y) ? "true" as i8* : "false" as i8*);
    printf("%d == %d: %s\n", x, z, (x == z) ? "true" as i8* : "false" as i8*);
    
    // Inequality  
    printf("%d != %d: %s\n", x, y, (x != y) ? "true" as i8* : "false" as i8*);
    printf("%d != %d: %s\n", x, z, (x != z) ? "true" as i8* : "false" as i8*);
    
    // Less than
    printf("%d < %d: %s\n", x, y, (x < y) ? "true" as i8* : "false" as i8*);
    printf("%d < %d: %s\n", y, x, (y < x) ? "true" as i8* : "false" as i8*);
    
    // Less than or equal
    printf("%d <= %d: %s\n", x, y, (x <= y) ? "true" as i8* : "false" as i8*);
    printf("%d <= %d: %s\n", x, z, (x <= z) ? "true" as i8* : "false" as i8*);
    
    // Greater than
    printf("%d > %d: %s\n", y, x, (y > x) ? "true" as i8* : "false" as i8*);
    printf("%d > %d: %s\n", x, y, (x > y) ? "true" as i8* : "false" as i8*);
    
    // Greater than or equal
    printf("%d >= %d: %s\n", y, x, (y >= x) ? "true" as i8* : "false" as i8*);
    printf("%d >= %d: %s\n", x, z, (x >= z) ? "true" as i8* : "false" as i8*);
}

// Test logical operations
func testLogical() {
    printf("=== Logical Operations ===\n");
    
    let t = true;
    let f = false;
    
    // Logical AND
    printf("true && true: %s\n", (t && t) ? "true" as i8* : "false" as i8*);
    printf("true && false: %s\n", (t && f) ? "true" as i8* : "false" as i8*);
    printf("false && true: %s\n", (f && t) ? "true" as i8* : "false" as i8*);
    printf("false && false: %s\n", (f && f) ? "true" as i8* : "false" as i8*);
    
    // Logical OR
    printf("true || true: %s\n", (t || t) ? "true" as i8* : "false" as i8*);
    printf("true || false: %s\n", (t || f) ? "true" as i8* : "false" as i8*);
    printf("false || true: %s\n", (f || t) ? "true" as i8* : "false" as i8*);
    printf("false || false: %s\n", (f || f) ? "true" as i8* : "false" as i8*);
}

// Test bitwise operations
func testBitwise() {
    printf("=== Bitwise Operations ===\n");
    
    let a = 12;  // 1100 in binary
    let b = 10;  // 1010 in binary
    
    // Bitwise AND
    let and_result = a & b;
    printf("%d & %d = %d\n", a, b, and_result);  // Should be 8 (1000)
    
    // Bitwise OR
    let or_result = a | b;
    printf("%d | %d = %d\n", a, b, or_result);   // Should be 14 (1110)
    
    // Bitwise XOR
    let xor_result = a ^ b;
    printf("%d ^ %d = %d\n", a, b, xor_result);  // Should be 6 (0110)
    
    // Left shift
    let lshift_result = a << 2;
    printf("%d << 2 = %d\n", a, lshift_result);  // Should be 48
    
    // Right shift
    let rshift_result = a >> 2;
    printf("%d >> 2 = %d\n", a, rshift_result);  // Should be 3
}

// Test mixed type operations
func testMixedTypes() {
    printf("=== Mixed Type Operations ===\n");
    
    let i8_val: i8 = 5;
    let i16_val: i16 = 300;
    let i32_val: i32 = 100000;
    let i64_val: i64 = 1000000000;
    
    // Test operations between different integer sizes
    let mixed1 = (i8_val as i32) + i32_val;
    printf("i8(%d) + i32(%d) = %d\n", i8_val as i32, i32_val, mixed1);
    
    let mixed2 = (i16_val as i32) * (i8_val as i32);
    printf("i16(%d) * i8(%d) = %d\n", i16_val as i32, i8_val as i32, mixed2);
    
    // Test unsigned operations
    let u8_val: u8 = 200;
    let u32_val: u32 = 50000;
    
    let unsigned_result = (u8_val as u32) + u32_val;
    printf("u8(%d) + u32(%d) = %d\n", u8_val as u32, u32_val, unsigned_result);
}

// Test short-circuit evaluation
func testShortCircuit() {
    printf("=== Short-Circuit Evaluation ===\n");
    
    let counter = 0;
    
    // Test AND short-circuit (should not call increment)
    if (false && (++counter > 0)) {
        printf("Should not reach here\n");
    }
    printf("Counter after false && expr: %d\n", counter);  // Should be 0
    
    // Test AND without short-circuit
    if (true && (++counter > 0)) {
        printf("AND condition met\n");
    }
    printf("Counter after true && expr: %d\n", counter);   // Should be 1
    
    // Test OR short-circuit (should not call increment)
    if (true || (++counter > 1)) {
        printf("OR condition met\n");
    }
    printf("Counter after true || expr: %d\n", counter);   // Should still be 1
    
    // Test OR without short-circuit
    if (false || (++counter > 1)) {
        printf("OR fallback condition met\n");
    }
    printf("Counter after false || expr: %d\n", counter);  // Should be 2
}

// Test operator precedence
func testPrecedence() {
    printf("=== Operator Precedence ===\n");
    
    // Test arithmetic precedence (* and / before + and -)
    let result1 = 2 + 3 * 4;
    printf("2 + 3 * 4 = %d\n", result1);  // Should be 14, not 20
    
    let result2 = 20 - 4 / 2;
    printf("20 - 4 / 2 = %d\n", result2); // Should be 18, not 8
    
    // Test comparison vs arithmetic
    let result3 = 5 + 3 > 4 * 2;
    printf("5 + 3 > 4 * 2: %s\n", result3 ? "true" as i8* : "false" as i8*); // 8 > 8 = false
    
    // Test logical vs comparison
    let result4 = 5 > 3 && 2 < 4;
    printf("5 > 3 && 2 < 4: %s\n", result4 ? "true" as i8* : "false" as i8*); // true && true = true
    
    // Test bitwise vs arithmetic
    let result5 = 4 + 2 & 3;
    printf("4 + 2 & 3 = %d\n", result5);  // (4 + 2) & 3 = 6 & 3 = 2
}

// Test associativity
func testAssociativity() {
    printf("=== Operator Associativity ===\n");
    
    // Left associative arithmetic
    let result1 = 20 - 5 - 3;
    printf("20 - 5 - 3 = %d\n", result1);  // Should be (20 - 5) - 3 = 12
    
    let result2 = 24 / 4 / 2;
    printf("24 / 4 / 2 = %d\n", result2);  // Should be (24 / 4) / 2 = 3
    
    // Right associative (logical operations should be left associative)
    let result3 = true && false || true;
    printf("true && false || true: %s\n", result3 ? "true" as i8* : "false" as i8*); // Should be true
}

// Test with pointer arithmetic (if supported)
func testPointerArithmetic() {
    printf("=== Pointer Arithmetic ===\n");
    
    let numbers: i32[5] = { 10, 20, 30, 40, 50 };
    let ptr = &numbers[0];
    
    // Test pointer + integer
    let ptr2 = ptr + 2;
    printf("*(ptr + 2) = %d\n", *ptr2);  // Should be 30
    
    // Test char pointer difference (only supported type)
    let str = "Hello";
    let char_ptr1 = &str[0];
    let char_ptr2 = &str[2];
    let diff = char_ptr2 - char_ptr1;
    printf("ptr_end - ptr_start = %d\n", diff as i32);  // Should be 2
}

func main() -> i32 {
    testArithmetic();
    testComparison();
    testLogical();
    testBitwise();
    testMixedTypes();
    testShortCircuit();
    testPrecedence();
    testAssociativity();
    testPointerArithmetic();
    
    printf("All binary expression tests completed\n");
    return 0;
}