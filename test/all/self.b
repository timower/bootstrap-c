// RUN: %bootstrap ../../src/bootstrap.b -o %t.1.ll
// RUN: clang %t.1.ll -o %t.bin
// RUN: %t.bin ../../src/bootstrap.b -o %t.2.ll
//
// RUN: diff %t.1.ll %t.2.ll
