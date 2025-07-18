// RUN: not %bootstrap -o 2>&1 | FileCheck --check-prefix=NO-OUTPUT %s
// RUN: not %bootstrap -target 2>&1 | FileCheck --check-prefix=NO-TARGET %s
// RUN: not %bootstrap file1.b file2.b 2>&1 | FileCheck --check-prefix=MULTIPLE %s
// RUN: not %bootstrap | FileCheck %s --check-prefix=NO-INPUT
// RUN: echo 'func main' | not %bootstrap -sema - 2>&1 | FileCheck --check-prefix=FAIL-SEMA %s
//
// Test command line argument parsing errors
//
// NO-OUTPUT: Expected output file after -o
// NO-TARGET: Expected target after -target
// MULTIPLE: Multiple input files not supported
// NO-INPUT: No input file specified
// FAIL-SEMA: stdin:2:1: : Expected: (
func main() -> i32 {
  return 0;
}
