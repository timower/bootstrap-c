// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli

// Test case to improve coverage of getEscaped function
// This test exercises different escape sequences

extern func printf(format: i8*, ...) -> i32;

func main() -> i32 {
    // Test various escape sequences to trigger getEscaped function
    // The getEscaped function handles: 'n', 't', 'r', '0', and default case
    
    // Test newline escape
    printf("Testing newline: line1\nline2\n");
    
    // Test tab escape
    printf("Testing tab: col1\tcol2\n");
    
    // Test carriage return escape
    printf("Testing CR: before\rafter\n");
    
    // Test null terminator escape
    printf("Testing null: text%cbefore null\n", '\0');
    
    // Test non-escape character (should pass through unchanged)
    printf("Testing backslash: \\ and quote: \"\n");
    
    // Test escape sequences in string literals
    let newline_str = "first line\nsecond line";
    let tab_str = "column1\tcolumn2";
    let mixed_str = "line1\nwith\ttab\rand\0null";
    
    printf("String with escapes: %s\n", newline_str);
    printf("Tab string: %s\n", tab_str);
    
    return 0;
}

// CHECK: define i32 @main()
// CHECK: call i32 @printf(ptr {{.*}}line1\0Aline2\0A{{.*}})
// CHECK: call i32 @printf(ptr {{.*}}col1\09col2\0A{{.*}})
// CHECK: call i32 @printf(ptr {{.*}}before\0Dafter\0A{{.*}})
// CHECK: call i32 @printf(ptr {{.*}}text\00cbefore null\0A{{.*}})
// CHECK: call i32 @printf(ptr {{.*}}backslash: \5C{{.*}})