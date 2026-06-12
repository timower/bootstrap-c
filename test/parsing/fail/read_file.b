// RUN: not %brio /asdf 2>&1 | FileCheck %s
// CHECK: open failed
// CHECK-NEXT: Failed to read input
// RUN: not %brio / 2>&1 | grep "read failed"
//
// RUN: %if system-linux %{ \
// RUN: not %brio /proc/self/comm 2>&1 | grep "seek failed" \
// RUN: %} %else %{ \
// RUN: %check-exit-code 1 %brio /dev/ttyub 2>&1 | grep "seek failed" \
// RUN: %}

