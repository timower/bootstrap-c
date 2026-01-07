// RUN: split-file %s %t
//
// RUN: not %bootstrap %t/unrelated_union.b 2>&1 | grep "Cannot cast union"
// RUN: not %bootstrap %t/unrelated_struct.b 2>&1 | grep "Cannot cast union"
// RUN: not %bootstrap %t/unknown_union1.b 2>&1 | grep "Can't cast struct to unrelated union"
// RUN: not %bootstrap %t/unknown_union2.b 2>&1 | grep "Cannot cast union"
// RUN: not %bootstrap %t/unknown_struct1.b 2>&1 | grep "Can't cast struct to unrelated union"
// RUN: not %bootstrap %t/unknown_struct2.b 2>&1 | grep "Cannot cast union"
// RUN: not %bootstrap %t/cant_cast.b 2>&1 | grep "Can't cast"
//--- unrelated_union.b
union Foo {
  A {}
}

union Bar {
  A {}
}

func test(x: Foo) {
  let y = x as Bar::A*;
}


//--- unrelated_struct.b
union Foo {
  A {}
}

struct A {}

func test(x: Foo) {
  let y = x as A*;
}


//--- unknown_union1.b
union Foo {
  A {}
}

func test(x: Foo::A) {
  let y = x as union Bar;
}


//--- unknown_union2.b
union Bar {
  A {}
}

func test(x: union Foo*) {
  let y = x as Bar::A*;
}


//--- unknown_struct1.b
struct A {}

union Bar {
  A {}
}

func test(x: struct A) {
  let y = x as Bar;
}


//--- unknown_struct2.b
struct Bar {}

func test(x: union Foo*) {
  let y = x as Bar*;
}


//--- cant_cast.b
let x = 12 as i8*;
