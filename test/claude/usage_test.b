// Test to trigger usage() function by providing invalid command line arguments
// RUN: not %bootstrap -o 2>&1 | FileCheck %s --check-prefix=CHECK-O
// RUN: not %bootstrap -target 2>&1 | FileCheck %s --check-prefix=CHECK-TARGET
// RUN: not %bootstrap file1.b file2.b 2>&1 | FileCheck %s --check-prefix=CHECK-MULTI

// CHECK-O: Expected output file after -o
// CHECK-O: Usage: bootstrap

// CHECK-TARGET: Expected target after -target
// CHECK-TARGET: Usage: bootstrap

// CHECK-MULTI: Multiple input files not supported
// CHECK-MULTI: Usage: bootstrap