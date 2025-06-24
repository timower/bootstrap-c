// Test to improve getEscaped() function coverage
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