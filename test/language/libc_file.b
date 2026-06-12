// REQUIRES: system-linux

// RUN: %brio %s -o %t.ll
// RUN: opt -S -p verify %t.ll
// RUN: lli %t.ll
// RUN: FileCheck %s --input-file=%t.ll
extern func fprintf(file: void*, fmt: i8*, ...) -> i32;

extern let stderr: void*;

func main() -> i32 {
  // CHECK: call i32 (ptr, ptr, ...) @fprintf
  fprintf(stderr, "test\n");
  return 0;
}
