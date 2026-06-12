// RUN: %brio -sema %s
func retInt[T]() {
  retInt:[i32]();
}

func n() {
  retInt:[i32];
}
