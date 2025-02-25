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
  // -24 in 32 bit two's complement
  const bar = 0b11111111111111111111111111101000;

  return foo + bar + baz;
}
