// RUN: not %brio -sema %s 2>&1 | grep "Too deep!"
func retInt[T]() -> typeof(retInt:[T]) {

}

func n() {
  retInt:[i32];
}
