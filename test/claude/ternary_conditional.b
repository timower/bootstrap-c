// RUN: %bootstrap %s | lli
// Test ternary conditional expressions (genConditional function)

func main() -> i32 {
  let a = 5;
  let b = 10;
  
  // Basic ternary conditional
  let max = a > b ? a : b;
  if (max != 10) {
    return 1;
  }
  
  // Nested ternary conditionals
  let result = a > b ? (a > 0 ? 1 : -1) : (b > 0 ? 2 : -2);
  if (result != 2) {
    return 2;
  }
  
  // Ternary with different types (should work with compatible types)
  let sign = a > 0 ? 1 : 0;
  if (sign != 1) {
    return 3;
  }
  
  // Boolean ternary
  let isPositive = a > 0 ? true : false;
  if (!isPositive) {
    return 4;
  }
  
  return 0;
}