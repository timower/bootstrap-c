// RUN: %bootstrap %s | lli
// Test pointers and arrays

func main() -> i32 {
    let value = 42;
    let ptr = &value;
    let deref = *ptr;
    
    let first = 1;
    let second = 2;
    let third = 3;
    
    let str = "Hello";
    
    return 0;
}