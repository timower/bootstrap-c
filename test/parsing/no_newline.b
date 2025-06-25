// Test for parsing files without trailing newline
// RUN: printf "func main() -> i32 {\n  return 0;\n}" > %t.b
// RUN: %bootstrap %t.b | FileCheck %s
// RUN: printf "func main() -> i32 {\n  return 0;\n}\n// FOO" > %t.b
// RUN: %bootstrap %t.b | FileCheck %s
//
// CHECK: define{{.*}}@main

// This test file content doesn't matter since we generate the actual test file

