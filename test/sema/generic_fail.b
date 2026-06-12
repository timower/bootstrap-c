// RUN: not %brio %s 2>&1 | grep "Can't resolve type tags, unknown parent type"
func test[T]() -> T::A {
  return T::A {};
}

func foo() {
  test:[i32]();
}
