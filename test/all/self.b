// RUN: %brio -stdlib %root_dir %root_dir/src/brio.b -o %t.1.ll
// RUN: clang %t.1.ll -o %t.bin
// RUN: %t.bin -stdlib %root_dir %root_dir/src/brio.b -o %t.2.ll
//
// RUN: cmp %t.1.ll %t.2.ll
