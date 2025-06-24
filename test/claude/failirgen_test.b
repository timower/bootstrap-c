// Test to trigger failIRGen() function with break outside loop
// RUN: not %bootstrap %s 2>&1 | FileCheck %s

func main() -> i32 {
    break;  // This should trigger "Break outside loop"
    return 0;
}

// CHECK: irgen fail: Break outside loop