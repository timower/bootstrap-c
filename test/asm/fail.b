// RUN: split-file %s %t
//
// RUN: not %brio -emit-asm %t/unhandled_instr.b 2>&1 | grep "Unhandled instruction"
// RUN: not %brio -emit-asm %t/unhandled_value.b 2>&1 | grep "Unhandled value type "
// RUN: not %brio -emit-asm %t/unhandled_binop.b 2>&1 | grep "Unhandled binary operation"
//
//--- unhandled_instr.b
func foo(x: i32) -> i32 {
  return 1 + x;
}

//--- unhandled_value.b
func foo() {

}

func test() -> func() {
  return foo;
}
//--- unhandled_binop.b
func foo() -> i32 {
  return 1 | 2;
}
