// REQUIRES: system-darwin

// RUN: %bootstrap %s -o %t.ll
// RUN: opt -S -p verify %t.ll
// RUN: lli %t.ll
extern func fprintf(file: void*, fmt: i8*, ...) -> i32;

extern let __stderrp: void*;

func main() -> i32 {
  fprintf(__stderrp, "test\n");
  return 0;
}
