// RUN: %bootstrap %s | lli
// Test arithmetic expressions in case statements

func main() -> i32 {
  let x = 10;
  switch (x) {
    case (5 + 5):
      return 0; // Success if arithmetic works
    case (2 + 3):
      return 2; // Different value (5)
    default:
      return 1; // Failure
  }
}