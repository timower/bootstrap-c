// RUN: %compile-and-run %s
func foo[T](do: bool, x: T*) {
  if (do)   defer (*x)++;
}

func main() -> i32 {
  let x = 0;
  foo:[i32](true, &x);
  foo:[i32](false, &x);
  return x - 1;
}
