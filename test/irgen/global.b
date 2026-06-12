// RUN: %brio %s | FileCheck %s
// CHECK: @x = constant i32
let x: const i32 = 0;

enum Foo {
  Zero,
}

let y = Foo::Zero;

let z = &y;

func foo() {

}

let w = &foo;
