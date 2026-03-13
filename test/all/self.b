// RUN: %bootstrap %root_dir/src/bootstrap.b -o %t.1.ll
// RUN: opt -S %t.1.ll
// RUN: clang %t.1.ll -o %t.bin
// RUN: %t.bin %root_dir/src/bootstrap.b -o %t.2.ll
//
// RUN: cmp %t.1.ll %t.2.ll
