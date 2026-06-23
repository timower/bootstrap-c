// RUN: %compile-and-run %s | FileCheck %s
// Test continue statement functionality in loops

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    printf("=== While Loop Continue Test ===\n");
    // CHECK: === While Loop Continue Test ===

    let i = 0;
    while (i < 5) {
        i++;
        if (i == 2 || i == 4) {
            continue;
        }
        printf("While: %d\n", i);
        // CHECK: While: 1
        // CHECK: While: 3
        // CHECK: While: 5
    }

    printf("=== For Loop Continue Test ===\n");
    // CHECK: === For Loop Continue Test ===

    for (let j = 0; j < 5; j++) {
        if (j == 1 || j == 3) {
            continue;
        }
        printf("For: %d\n", j);
        // CHECK: For: 0
        // CHECK: For: 2
        // CHECK: For: 4
    }

    printf("=== Nested Loop Continue Test ===\n");
    // CHECK: === Nested Loop Continue Test ===

    for (let outer = 0; outer < 3; outer++) {
        for (let inner = 0; inner < 3; inner++) {
            if (inner == 1) {
                continue;
            }
            printf("Nested: %d,%d\n", outer, inner);
            // CHECK: Nested: 0,0
            // CHECK: Nested: 0,2
            // CHECK: Nested: 1,0
            // CHECK: Nested: 1,2
            // CHECK: Nested: 2,0
            // CHECK: Nested: 2,2
        }
    }

    return 0;
}
