// Test to improve getEscaped() function coverage
// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    // Test all escape sequences handled by getEscaped function
    printf("Testing newline: before\\nafter\\n");
    printf("Testing tab: before\\tafter\\n");
    printf("Testing carriage return: before\\rafter\\n");
    printf("Testing null character: before");
    printf("%c", '\\0');
    printf("after\\n");
    
    // Test escaped backslash and quotes (default case)
    printf("Testing backslash: \\\\\\n");
    printf("Testing quote: \"hello\"\\n");
    printf("Testing single quote: 'hello'\\n");
    
    // Test other characters that go through default case
    printf("Testing other: \\a \\b \\f \\v\\n");
    
    return 0;
}

// CHECK: define i32 @main()
// CHECK: call i32 @printf(ptr {{.*}}before\\0Aafter\\0A{{.*}})
// CHECK: call i32 @printf(ptr {{.*}}before\\09after\\0A{{.*}})
// CHECK: call i32 @printf(ptr {{.*}}before\\0Dafter\\0A{{.*}})
// CHECK: call i32 @printf(ptr {{.*}}\\5C\\0A{{.*}})