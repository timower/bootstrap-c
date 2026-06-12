// RUN: %brio %s | lli
//
func test() -> i32 {
  let x = 1;
  defer x = 0;
  return x * 1;
}

func main() -> i32 {
  return test() - 1;
}
