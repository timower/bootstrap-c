// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli

// Test case to improve coverage of isUnary function
// Tests various unary operators to ensure all branches are covered

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    let x = 42;
    let ptr = &x;
    
    // Test bitwise NOT operator (~) - should trigger TILDE case
    let bitwise_not = ~x;
    
    // Test logical NOT operator (!) - should trigger BANG case  
    let logical_not = !false;
    
    // Test increment operators (++) - should trigger INC_OP case
    let pre_inc = ++x;
    let post_inc = x++;
    
    // Test decrement operators (--) - should trigger DEC_OP case
    let pre_dec = --x;
    let post_dec = x--;
    
    // Test address-of operator (&) - should trigger AND case
    let addr = &x;
    
    // Test dereference operator (*) - should trigger STAR case
    let deref = *ptr;
    
    // Test unary plus (+) - should trigger PLUS case
    let unary_plus = +x;
    
    // Test unary minus (-) - should trigger MINUS case
    let unary_minus = -x;
    
    printf("Unary operations test:\n");
    printf("Bitwise NOT: %d\n", bitwise_not);
    printf("Logical NOT: %d\n", logical_not);
    printf("Increments: %d %d\n", pre_inc, post_inc);
    printf("Decrements: %d %d\n", pre_dec, post_dec);
    printf("Dereference: %d\n", deref);
    printf("Unary plus/minus: %d %d\n", unary_plus, unary_minus);
    
    return 0;
}

// CHECK: define i32 @main()
// CHECK: xor i32 {{.*}}, -1
// CHECK: add i32 {{.*}}, 1
// CHECK: sub i32 {{.*}}, 1
// CHECK: alloca i32
// CHECK: load ptr
// CHECK: sub i32 0, {{.*}}