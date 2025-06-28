// RUN: %bootstrap %s | FileCheck %s
// CHECK: @y = global [3 x i32] [ i32 1, i32 2, i32 3 ]
// CHECK: @x = global [3 x ptr]
let x = { "A", "B", "C"};

let y = { 1, 2, 3};
