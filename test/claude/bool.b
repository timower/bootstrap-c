// RUN: %bootstrap %s -o %t.ll
// RUN: FileCheck %s < %t.ll
// RUN: lli %t.ll
// Test bool type and logical operations - should return 0 if logic works correctly

// CHECK: alloca i1
// CHECK: store i1 1
// CHECK: store i1 0
// CHECK: load i1
// CHECK: br i1
// CHECK: icmp

func main() -> i32 {
    let trueVal = true;
    let falseVal = false;
    
    if (trueVal && falseVal) {
        return 1;
    }
    
    if (trueVal || falseVal) {
        let a = 10;
        let b = 5;
        if (a > b && a >= b && a != b) {
            if (!(a < b) && !(a <= b) && !(a == b)) {
                return 0;
            }
        }
    }
    
    return 2;
}