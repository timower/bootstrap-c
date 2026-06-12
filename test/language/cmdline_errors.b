// RUN: not %brio -o 2>&1 | FileCheck --check-prefix=NO-OUTPUT %s
// RUN: not %brio -target 2>&1 | FileCheck --check-prefix=NO-TARGET %s
// RUN: not %brio file1.b file2.b 2>&1 | FileCheck --check-prefix=MULTIPLE %s
// RUN: not %brio | FileCheck %s --check-prefix=NO-INPUT
// RUN: not %brio -stdin-filename | grep "Expected filename"
//
// RUN: not %brio -target arm32-unknonw-linux-gnu 2>&1 | grep 'Failed to parse arch'
// RUN: not %brio -target armv7l 2>&1 | grep 'Expected -'
// RUN: not %brio -target armv7l-fooo-gnu 2>&1 | grep 'Failed to parse platform'
// RUN: not %brio -target armv7l-w64-gnul 2>&1 | grep 'Failed to parse abi'
//
// RUN: not %brio %s -o / 2>&1 | grep 'Failed to open output file'
//
// RUN: %if system-linux %{ \
// RUN: not %brio - 2>&1 </proc/self/mem | grep 'Read Failed' \
// RUN: %} %else %{ \
// RUN: not %brio - 2>&1 </dev/aes_0 | grep 'Read Failed' \
// RUN: %}
//
// RUN: cat %s | not %brio -format -i - 2>&1 | grep "Cannot use -i with stdin input"
// RUN: not %brio -format -i -o test %s 2>&1 | grep "Cannot use both -i and -o"
//
// RUN: echo 'func main' | not %brio -sema - 2>&1 | FileCheck --check-prefix=FAIL-SEMA %s
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
