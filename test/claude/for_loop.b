// RUN: %bootstrap %s | lli
// Test for loop statements (parseForStmt function)

func main() -> i32 {
  // Basic for loop
  let sum = 0;
  for (let i = 0; i < 5; i++) {
    sum = sum + i;
  }
  if (sum != 10) {
    return 1;
  }
  
  // For loop with different step
  let product = 1;
  for (let j = 2; j <= 8; j = j + 2) {
    product = product * j;
  }
  if (product != 384) { // 1 * 2 * 4 * 6 * 8 = 384
    return 2;
  }
  
  // For loop with break
  let count = 0;
  for (let k = 0; k < 100; k++) {
    count++;
    if (k == 3) {
      break;
    }
  }
  if (count != 4) {
    return 3;
  }
  
  // Nested for loops
  let nested_sum = 0;
  for (let x = 1; x <= 2; x++) {
    for (let y = 1; y <= 2; y++) {
      nested_sum = nested_sum + (x * y);
    }
  }
  if (nested_sum != 9) {
    return 4;
  }
  
  return 0;
}