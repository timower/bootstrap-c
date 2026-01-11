// RUN: split-file %s %t
// RUN: not %bootstrap %t/no-const.b 2>&1 | grep "TODO"
// RUN: not %bootstrap %t/slice-idx.b 2>&1 | grep "Constant GEP"
//
// RUN: not %bootstrap %t/lvalue1.b 2>&1 | grep "Expr can't be used as lvalue"
// RUN: not %bootstrap %t/lvalue2.b 2>&1 | grep "Expr can't be used as lvalue"
// RUN: not %bootstrap %t/lvalue3.b 2>&1 | grep "Expr can't be used as lvalue"
//
// RUN: not %bootstrap %t/sizeof_unknown.b 2>&1 | grep "Unkown type"
// RUN: not %bootstrap %t/break.b 2>&1 | grep "Break outside loop"
// RUN: not %bootstrap %t/continue.b 2>&1 | grep "Continue outside loop"
// RUN: not %bootstrap %t/slice_end.b 2>&1 | grep "Constant GEP"
//
// RUN: not %bootstrap %t/const_cast.b 2>&1 | grep "TODO"
// RUN: not %bootstrap %t/const_unary.b 2>&1 | grep "TODO"
// RUN: not %bootstrap %t/const_addr.b 2>&1 | grep "TODO"
// RUN: not %bootstrap %t/const_struct.b 2>&1 | grep "TODO"
//
//--- no-const.b
let x = 12;

let z = ++x;


//--- lvalue1.b
func foo() {
  1 = 2;
}


//--- lvalue2.b
func buz() -> i32* {
  return &+1;
}


//--- lvalue3.b
func z() -> i32 {
  return 1;
}

func lvalue3() -> i32* {
  return &z();
}


//--- slice-idx.b
let u = [ 1, 2, 3 ];

let y = (&u)[1:];



//--- slice_end.b
let s: [i8] = "test"[:][:];


//--- sizeof_unknown.b
let c = sizeof(struct Tag);


//--- break.b
func bar() {
  break;
}


//--- continue.b
func baz() {
  continue;
}


//--- const_cast.b
const f = 12;

let x = f as u64;


//--- const_unary.b
let h = 12;

let g = &h;

let f = *g;


//--- const_addr.b
let g = &(1 + 2);


//--- const_struct.b
struct Foo {}

let g = Foo {};
