// RUN: not %bootstrap /asdf 2>&1 | grep "open failed"
// RUN: not %bootstrap / 2>&1 | grep "read failed"
//
// RUN: %if system-linux %{ \
// RUN: not %bootstrap /proc/self/comm 2>&1 | grep "seek failed" \
// RUN: %} %else %{ \
// RUN: not %bootstrap /dev/tty 2>&1 | grep "seek failed" \
// RUN: %}

