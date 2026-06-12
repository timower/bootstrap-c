// RUN: split-file %s %t
//
// RUN: not %brio %t/slice1.b  2>&1 | grep "Arg type mismatch"
// RUN: not %brio %t/slice2.b  2>&1 | grep "Arg type mismatch"
// RUN: not %brio %t/slice3.b  2>&1 | grep "Arg type mismatch"
//
// RUN: not %brio %t/ptr1.b  2>&1 | grep "Arg type mismatch"
// RUN: not %brio %t/ptr2.b  2>&1 | grep "Arg type mismatch"
//
// RUN: not %brio %t/union.b  2>&1 | grep "Arg type mismatch"
//
//--- slice1.b
func slice1(x: [i32]) {

}

func a() {
  slice1(null);
}


//--- slice2.b
func slice2(x: [i32]) {

}

func b() {
  let x = [ 1 as i64 ];
  slice2(&x);
}


//--- slice3.b
func slice3(x: [i32]) {

}

func f() {
  slice3(12);
}


//--- ptr1.b
func ptr1(x: i32*) {

}

func c(x: i64*) {
  ptr1(x);
}


//--- ptr2.b
func d(x: i64*) {

}

func ptr2(x: i64[0]*) {
  d(x);
}


//--- union.b
union Foo {}

func un(x: Foo) {

}

func e(x: i32) {
  un(x);
}
