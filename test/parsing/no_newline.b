// Test for parsing files without trailing newline
//
// RUN: printf "func main() -> i32 {\n  return 0;\n}" > %t.b
// RUN: %bootstrap -format %t.b | FileCheck %s
//
// RUN: printf "func main() -> i32 {\n  return 0;\n}\n// FOO" > %t.b
// RUN: %bootstrap -format %t.b | FileCheck --check-prefix=COMMENT %s

// This test file content doesn't matter since we generate the actual test file

// CHECK: func main() -> i32 {
// CHECK-NEXT: return 0;
// CHECK-NEXT: }
//
// COMMENT: func main() -> i32 {
// COMMENT-NEXT: return 0;
// COMMENT-NEXT: }
// COMMENT-NEXT: // FOO

