// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli
// Test pointers and arrays

// CHECK: alloca i32
// CHECK: alloca ptr
// CHECK: store i32 42
// CHECK: store ptr
// CHECK: load ptr
// CHECK: load i32

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