// RUN: %bootstrap %s | FileCheck %s
//
// CHECK: define %struct.Bar.A
// CHECK: icmp eq i32 {{.*}}, 2
//
// CHECK: define %struct.Foo.A
// CHECK: icmp eq i32 {{.*}}, 0
union Foo {
  A {}
  B {}
  C {}
}

union Bar {
  C {}
  B {}
  A {}
}

func getA[T](x: T) -> T::A {
  return *(x as T::A*);
}

func test() {
  let a = getA:[Foo](Foo::A {});
  let b = getA:[Bar](Bar::A {});
}
