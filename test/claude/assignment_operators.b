// Test to improve genAssign() function coverage
// RUN: %bootstrap %s | lli

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    let a = 10;
    let b = 5;
    let c = 2;
    let d = 8;
    let e = 15;
    let f = 3;
    let g = 7;
    let h = 4;
    
    // Test all assignment operators to improve genAssign coverage
    a += b;    // ADD_ASSIGN
    printf("a += b: %d\n", a);
    
    a -= c;    // SUB_ASSIGN
    printf("a -= c: %d\n", a);
    
    a *= c;    // MUL_ASSIGN
    printf("a *= c: %d\n", a);
    
    a /= f;    // DIV_ASSIGN
    printf("a /= f: %d\n", a);
    
    d %= b;    // MOD_ASSIGN
    printf("d %%= b: %d\n", d);
    
    e <<= 1;   // LEFT_ASSIGN
    printf("e <<= 1: %d\n", e);
    
    e >>= 2;   // RIGHT_ASSIGN
    printf("e >>= 2: %d\n", e);
    
    g &= h;    // AND_ASSIGN
    printf("g &= h: %d\n", g);
    
    g |= b;    // OR_ASSIGN
    printf("g |= b: %d\n", g);
    
    g ^= c;    // XOR_ASSIGN
    printf("g ^= c: %d\n", g);
    
    // Basic assignment for comparison
    let result = a;
    result = b + c;  // EQ assignment
    printf("result = b + c: %d\n", result);
    
    return 0;
}