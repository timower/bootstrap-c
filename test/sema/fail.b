// RUN: split-file %s %t
//
// RUN: not %bootstrap %t/var.b 2>&1 | grep "Couldn't find variable"
// RUN: not %bootstrap %t/noend.b 2>&1 | grep "Pointer to slice requires end"
// RUN: not %bootstrap %t/cond.b 2>&1 | grep "lhs and rhs should have same type"
// RUN: not %bootstrap %t/noparent.b 2>&1 | grep "Parent type not found"
//
// RUN: not %bootstrap %t/only_len1.b 2>&1 | grep "Only 'len' member"
// RUN: not %bootstrap %t/only_len2.b 2>&1 | grep "Only 'len' member"
//
// RUN: not %bootstrap %t/no_type.b 2>&1 | grep "Unknown type for scope expr"
// RUN: not %bootstrap %t/sizeof_unknown1.b 2>&1 | grep "Unkown type to get size of"
// RUN: not %bootstrap %t/sizeof_unknown2.b 2>&1 | grep "Unkown type to get size of"
// RUN: not %bootstrap %t/import1.b 2>&1 | grep "Error in import"
// RUN: not %bootstrap %t/import2.b 2>&1 | grep "Failed to import file"
// RUN: not %bootstrap %t/missing_import.b 2>&1 | grep "Couldn't find file"
// RUN: not %bootstrap %t/redef.b 2>&1 | grep "Variable redef"
//
// RUN: not %bootstrap %t/eval1.b 2>&1 | grep "Not a constant expression"
// RUN: not %bootstrap %t/eval2.b 2>&1 | grep "Division by zero"
// RUN: not %bootstrap %t/eval3.b 2>&1 | grep "Modulo by zero"
//
// RUN: not %bootstrap %t/non_struct.b 2>&1 | grep "Expected struct type"
// RUN: not %bootstrap %t/no_struct.b 2>&1 | grep "Expected struct type"
// RUN: not %bootstrap %t/slice1.b 2>&1 | grep "Start expression must be integer"
// RUN: not %bootstrap %t/slice2.b 2>&1 | grep "End expression must be integer"
// RUN: not %bootstrap %t/array.b 2>&1 | grep "Init must have consistent type"
// RUN: not %bootstrap %t/non_func.b 2>&1 | grep "Must call function or function pointer type"
// RUN: not %bootstrap %t/member.b 2>&1 | grep "Unknown type for member"
// RUN: not %bootstrap %t/unary_ptr.b 2>&1 | grep "Unary on pointer"
// RUN: not %bootstrap %t/struct_convert.b 2>&1 | grep "cannot convert to field type"
// RUN: not %bootstrap %t/no_field.b 2>&1 | grep "cannot find field"
// RUN: not %bootstrap %t/no_enum.b 2>&1 | grep "Cannot find enum value"
// RUN: not %bootstrap %t/no_member.b 2>&1 | grep "Cannot find field"
// RUN: not %bootstrap %t/member_no_struct.b 2>&1 | grep "Expected struct type"
//
// RUN: not %bootstrap %t/decl1.b 2>&1 | grep "Decl init type doesn't match"
// RUN: not %bootstrap %t/decl2.b 2>&1 | grep "Const decl must have an init"
//
// RUN: not %bootstrap %t/unknown_parent.b 2>&1 | grep "Can't resolve type tags, unknown parent"
// RUN: not %bootstrap %t/unknown_sub.b 2>&1 | grep "Can't resolve type tags, unknown sub"
// RUN: not %bootstrap %t/unknown_type.b 2>&1 | grep "Can't resolve type tags, unknown type"
//
// RUN: not %bootstrap %t/no_void.b 2>&1 | grep "Return type should be void"
// RUN: not %bootstrap %t/ret_mismatch.b 2>&1 | grep "Return type mismatch"
//
// RUN: not %bootstrap %t/bin_ptr.b 2>&1 | grep "Only integers"
// RUN: not %bootstrap %t/bin_types1.b 2>&1 | grep "Binary op on different types"
// RUN: not %bootstrap %t/bin_types2.b 2>&1 | grep "Only integers"
// RUN: not %bootstrap %t/bin_types3.b 2>&1 | grep "Unsupported type for compare"
// RUN: not %bootstrap %t/bin_types4.b 2>&1 | grep "type mismatch"
// RUN: not %bootstrap %t/wrong_assign.b 2>&1 | grep "Assign doesn't match"
//
// RUN: not %bootstrap %t/type_redef.b 2>&1 | grep "Type redef"
//
// RUN: not %bootstrap %t/index1.b 2>&1 | grep "Index only works on arrays"
// RUN: not %bootstrap %t/index2.b 2>&1 | grep "Can't index with non integer"
//
// RUN: not %bootstrap %t/no_ptr_member.b 2>&1 | grep "Expected pointer for ->"
//
// RUN: not %bootstrap %t/generics1.b 2>&1 | grep "Failed to instantiate"
// RUN: not %bootstrap %t/generics2.b 2>&1 | grep "Couldn't find variable in scope"
// RUN: not %bootstrap %t/not_generic1.b 2>&1 | grep "Expected generic function type"
// RUN: not %bootstrap %t/not_generic2.b 2>&1 | grep "Expected function declaration"
//
// RUN: not %bootstrap %t/no_init.b 2>&1 | grep "Let expression must have an init"
//
// RUN: not %bootstrap %t/no_union.b 2>&1 | grep "Expected union type"
// RUN: not %bootstrap %t/wrong_slice.b 2>&1 | grep "Expected slice or array"
// RUN: not %bootstrap %t/arg_type.b 2>&1 | grep "Arg type mismatch"
// RUN: not %bootstrap %t/if_let.b 2>&1 | grep "Expected bool"
//
// RUN: not %bootstrap %t/const_assign.b 2>&1 | grep "Assign expression not supported"
//
// RUN: not %bootstrap %t/recursive_type.b 2>&1 | grep "Recursive type declaration!"
//
// RUN: not %bootstrap %t/not_union.b 2>&1 | grep " Can't resolve type tags, unknown sub type"
// RUN: not %bootstrap %t/not_array.b 2>&1 | grep "Expected array init for array declaration"
// RUN: not %bootstrap %t/not_ptr.b 2>&1 | grep "Expected pointer init for pointer declaration"
// RUN: not %bootstrap %t/broken_for.b 2>&1 | grep "For condition must be a boolean expression"
//
// RUN: not %bootstrap %t/uninst_generic.b 2>&1 | grep "Uninstantiated generic expression"
//
// RUN: not %bootstrap %t/unsized.b 2>&1 | grep "Decl with unsized type must have init"
//
//--- var.b
let x = y;


//--- noend.b
let x = null[:];


//--- generics1.b
func x[T]() {

}

func foo() {
  x:[i32, i32]();
}


//--- generics2.b
func bar() {
  x:[i32, i32]();
}


//--- cond.b
let a = true ? "foo" : 1;


//--- noparent.b
let x = Foo::A {};


//--- only_len1.b
func a(x: [i8]) -> i32 {
  return x.foo;
}


//--- only_len2.b
func a(x: i8[2]) -> i32 {
  return x.foobar;
}


//--- no_type.b
let x = Foo::A;


//--- sizeof_unknown1.b
let x = sizeof(struct Foo);


//--- sizeof_unknown2.b
let x = sizeof(union Foo);


//--- import1.b
import missing_ret;


//--- import2.b
import empty;


//--- empty.b
//--- missing_ret.b
func foo() -> i32 {

}


//--- missing_import.b
import missing_file;


//--- redef.b
func foo() {
  let x = 1;
  let x = 2;
}


//--- eval1.b
const x = true ? 1 : 2;


//--- eval2.b
const x = 5 / 0;


//--- eval3.b
const x = 8 % 0;


//--- non_struct.b
union Foo {}

let x = Foo {};


//--- no_struct.b
func foo() {
  let x = Foo {};
}


//--- slice1.b
let x = null[true:2];


//--- slice2.b
let x = null[:false];


//--- array.b
let x = [ 1, true ];


//--- non_func.b
let x = false(12);


//--- member.b
func foo(x: struct Foo) -> i32 {
  return x.y;
}


//--- member_no_struct.b
func foo(x: union Foo) -> i32 {
  return x.y;
}


//--- unary_ptr.b
let x = null++;


//--- struct_convert.b
struct Foo {
  b: bool;
}

let x = Foo {
  b = 12,
};


//--- no_field.b
struct Foo {}

let x = Foo {
  b = 12,
};


//--- no_enum.b
enum Foo {
}

let x = Foo::A;


//--- decl1.b
let x: bool = 12;


//--- decl2.b
const x: bool;


//--- unknown_parent.b
func x() -> A::B {

}


//--- unknown_sub.b
union A {}

func x() -> A::B {

}


//--- unknown_type.b
func x() -> A {

}


//--- no_void.b
func foo() -> i32 {
  return;
}


//--- ret_mismatch.b
func foo() -> i8* {
  return 12;
}


//--- bin_ptr.b
let x = null + 12;


//--- bin_types1.b
let x = "" < 12;


//--- bin_types2.b
func foo(y: struct Foo) {
  let x = y + 12;
}


//--- bin_types3.b
func foo(x: struct Foo, y: struct Foo) -> bool {
  return x == y;
}


//--- bin_types4.b
func foo(x: i32, y: bool) -> i32 {
  return x + y;
}


//--- wrong_assign.b
func foo(x: bool) {
  let y: void* = null;
  y = x;
}


//--- type_redef.b
struct Foo {}

enum Foo {
}

union Foo {}


//--- index1.b
let x = null[2];


//--- index2.b
let foo = [ 1, 2 ];

let x = foo["test"];


//--- no_ptr_member.b
struct Struct {}

let x = Struct {};

let y = x->a;


//--- not_generic1.b
func foo() {

}

func bar() {
  foo:[i32]();
}


//--- not_generic2.b
let foo = 12;

func bar() {
  foo:[i32]();
}


//--- no_member.b
struct A {}

let x = A {};

let y = x.a;


//--- no_init.b
func foo() {
  let x: i32;
}


//--- no_union.b
struct Foo {}

let x = Foo::A {};


//--- wrong_slice.b
let x = false[:1];


//--- arg_type.b
func foo(x: bool) {

}

func bar() {
  foo(null);
}


//--- if_let.b
func foo() -> i32 {
  if (let x = 12) {
    return x;
  }
  return 0;
}


//--- const_assign.b
const broken = 1 <<= 2;


//--- recursive_type.b
struct Point {
  d: Point;
}

union e {
  e {
    t: Point;
  }
}


//--- not_union.b
enum Foo {
}

func c(x: Foo::B) {

}


//--- not_array.b
let r: i64[] = 1;


//--- not_ptr.b
let r: i64[]* = 1;


//--- broken_for.b
func a() {
  for (1; ; 1) {

  }
}


//--- uninst_generic.b
func retVoid[T]() {

}

func a() {
  retVoid;
}


//--- unsized.b
let x: [i32[]]*;
