// RUN: not %bootstrap %s 2>&1 | grep "TODO: mono"
func foo[T]() -> i32 {
  const x: T = 12;
  return x;
}

func main() -> i32 {
  return foo:[i32]();
}
