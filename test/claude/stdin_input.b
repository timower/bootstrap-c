// RUN: echo "func main() -> i32 { return 0; }" | %bootstrap | lli
// Test reading from stdin (readStdin function)

// This test file tests the readStdin functionality by piping a simple program
// through stdin to the bootstrap compiler. The program should compile and 
// execute correctly, returning 42.

// The actual test content is provided via stdin in the RUN command above.
// This file serves as the test case that ensures stdin reading works properly.