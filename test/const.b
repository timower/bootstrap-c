// RUN: %bootstrap %s -o %t.ll
// RUN: opt -S -p verify %t.ll
// RUN: lli %t.ll
//
// RUN: FileCheck %s < %t.ll
// CHECK-NOT: @foo =
// CHECK-NOT: store i32 -12
const foo = 0xC;

const baz = 0o14;

func main() -> const i32 {
  const bar = 0b11;
  return foo + bar + baz - 27;
}
