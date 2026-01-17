// RUN: not %bootstrap /asdf 2>&1 | FileCheck %s
// CHECK: open failed
// CHECK-NEXT: Failed to read input
// RUN: not %bootstrap / 2>&1 | grep "read failed"
//
// RUN: %if system-linux %{ \
// RUN: not %bootstrap /proc/self/comm 2>&1 | grep "seek failed" \
// RUN: %} %else %{ \
// RUN: %check-exit-code 1 %bootstrap /dev/ttyub 2>&1 | grep "seek failed" \
// RUN: %}

