// RUN: %bootstrap %s | lli
// Test constant array expressions to improve genConstant coverage
func main() -> i32 {
  let arr: i32[3] = { (1 + 2), (3 * 4), (5 - 2)};
  return arr[0] + arr[1] + arr[2] - 18;
}

