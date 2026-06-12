/// Tests all types that are not modified by generics.
//
// RUN: %brio %s -o %t.ll
// RUN: FileCheck %s --input-file %t.ll
//
// CHECK-DAG: define void @retVoid
func retVoid[T]() {

}


// CHECK-DAG: define i1 @retBool
func retBool[T]() -> bool {
  return sizeof(T) > 0;
}


// CHECK-DAG: define i32 @retInt
func retInt[T]() -> u32 {
  return 0;
}

enum Foo {
  A,
}


// CHECK-DAG: define i32 @retEnum
func retEnum[T]() -> enum Foo {
  return Foo::A;
}

struct Bar {}


// CHECK-DAG: define %struct.Bar @retStruct
func retStruct[T]() -> struct Bar {
  return Bar {};
}

union Buz {
  A {}
}


// CHECK-DAG: define %union.Buz @retUnion
func retUnion[T]() -> union Buz {
  return Buz::A {};
}


// CHECK-DAG: define i32 @retTypeof
func retTypeof[T]() -> typeof(retInt:[T]()) {
  return 5;
}

func main() {
  retVoid:[i32]();
  retBool:[i32]();
  retInt:[i32]();
  retEnum:[i32]();
  retStruct:[i32]();
  retUnion:[i32]();
  retTypeof:[i32]();
}
