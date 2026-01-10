// RUN: split-file %s %t
//
// canCast is true if 'from `typeEq` to '
//
// RUN: not %bootstrap %t/arr1.b 2>&1 | grep "Can't cast"
// RUN: not %bootstrap %t/arr2.b 2>&1 | grep "Can't cast"
// RUN: not %bootstrap %t/slice.b 2>&1 | grep "Can't cast"
// RUN: not %bootstrap %t/struct1.b 2>&1 | grep "Can't cast"
// RUN: not %bootstrap %t/struct2.b 2>&1 | grep "Can't cast"
//
// RUN: not %bootstrap %t/func1.b 2>&1 | grep "Can't cast"
// RUN: not %bootstrap %t/func2.b 2>&1 | grep "Can't cast"
// RUN: not %bootstrap %t/func3.b 2>&1 | grep "Can't cast"
// RUN: not %bootstrap %t/func4.b 2>&1 | grep "Can't cast"
// RUN: not %bootstrap %t/func5.b 2>&1 | grep "Can't cast"
// RUN: not %bootstrap %t/func6.b 2>&1 | grep "Can't cast"
//
//--- arr1.b
func arr1(x: i8[3]) {
  let y = x as i8[2];
}


//--- arr2.b
func arr2(x: u16[2]) {
  let y = x as i8[2];
}


//--- slice.b
func slice(x: [u16]) {
  let y = x as i8[2];
}


//--- struct1.b
union Foo {
  Bar {}
}

struct Bar {}

func struct1(x: Foo::Bar) {
  let y = x as Bar;
}


//--- struct2.b
union Baz {
  Foo {}
}

union Buz {
  Foo {}
}

func struct2(x: Baz::Foo) {
  let y = x as Buz::Foo;
}


//--- func1.b
func func1(x: func() -> i32) {
  let y = x as func() -> i64;
}


//--- func2.b
func func2(x: func(...) -> i32) {
  let y = x as func() -> i32;
}


//--- func3.b
func func3(x: func(i32) -> i32) {
  let y = x as func(i64) -> i32;
}


//--- func4.b
func func4(x: func(i32, i32) -> i32) {
  let y = x as func(i32) -> i32;
}


//--- func5.b
func func5(x: func(i32) -> i32) {
  let y = x as func(i32, i32) -> i32;
}


//--- func6.b
func func6(x: func(i32) -> i32) {
  let y = x as i32;
}
