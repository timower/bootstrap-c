// RUN: %bootstrap %s -o %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file %t.ll
