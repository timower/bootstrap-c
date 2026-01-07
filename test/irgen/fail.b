// RUN: split-file %s %t
// RUN: not %bootstrap %t/no-const.b 2>&1 | grep "TODO"
// RUN: not %bootstrap %t/slice-idx.b 2>&1 | grep "Constant GEP"
// RUN: not %bootstrap %t/lvalue.b 2>&1 | grep "Expr can't be used as lvalue"
// RUN: not %bootstrap %t/sizeof_unsized.b 2>&1 | grep "Unsized array in sizeof"
// RUN: not %bootstrap %t/sizeof_unknown.b 2>&1 | grep "Unkown type"
// RUN: not %bootstrap %t/break.b 2>&1 | grep "Break outside loop"
// RUN: not %bootstrap %t/continue.b 2>&1 | grep "Continue outside loop"
//
//--- no-const.b
let x = 12;

let z = ++x;


//--- lvalue.b
func foo() {
  1 = 2;
}


//--- slice-idx.b
let x = [ 1, 2, 3 ];

let y = (&x)[1:];


//--- sizeof_unsized.b
let x = sizeof(i8[]);


//--- sizeof_unknown.b
let x = sizeof(struct Tag);


//--- break.b
func foo() {
  break;
}


//--- continue.b
func foo() {
  continue;
}
