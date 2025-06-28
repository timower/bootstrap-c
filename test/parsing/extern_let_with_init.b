// RUN: not %bootstrap -sema %s 2>&1 | FileCheck %s
// Test extern let with initializer error

// CHECK: Extern let cannot have init
extern let invalidExternWithInit: i32 = 42;

func main() -> i32 {
    return 0;
}