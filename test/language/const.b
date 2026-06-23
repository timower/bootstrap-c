// RUN: %compile-and-run %s %t.ll
// RUN: FileCheck %s < %t.ll
//
// CHECK-NOT: @foo =
// CHECK-NOT: store i32 -12
// CHECK: add i32
// CHECK: ret i32
const foo = 0xC;

const baz = 0o14;

const buz = 0xa;

const biz = 0x11;

func main() -> const i32 {
  const bar = 0b11;
  return foo + bar + baz + buz + biz - 37 - 17;
}
