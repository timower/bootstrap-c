// Test to improve getLineComments() function coverage by testing various comment scenarios
// RUN: %bootstrap -format %s -o %t.b
// RUN: diff %s %t

extern func printf(format: i8*, ...) -> i32;

// Comment on line 1
func testFunction() -> i32 { // End of line comment
    // Comment on line 2
    let a = 10; // Another end of line comment
    // Multiple
    // consecutive
    // comments
    let b = 20; // Comment after variable

    // Comment before return
    return a + b; // Final comment
} // Function end comment

// Another standalone comment
struct TestStruct {
    x: i32; // Field comment
    // Comment between fields
    y: i32; // Another field comment
};

func main() -> i32 {
    // Comment in main function
    let result = testFunction(); // Call function
    printf("Result: %d\\n", result); // Print result
    // Final comment
    return 0;
}
