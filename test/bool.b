// RUN: %bootstrap %s | lli
func main() -> i32 {
  let trueV = true;
  let falseV = false;
  if (trueV && falseV) {
    return 1;
  }
  if (trueV || falseV) {
    return 0;
  }
  return 2;
}
