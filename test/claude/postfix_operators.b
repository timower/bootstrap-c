// RUN: %bootstrap %s | lli
// Test postfix unary operators (parseUnaryPostfix function)

func main() -> i32 {
  // Basic postfix increment
  let x = 5;
  let old_x = x++;
  if (old_x != 5 || x != 6) {
    return 1;
  }
  
  // Basic postfix decrement
  let y = 10;
  let old_y = y--;
  if (old_y != 10 || y != 9) {
    return 2;
  }
  
  // Postfix in expressions
  let a = 3;
  let result = a++ + a;
  if (result != 7 || a != 4) {
    return 3;
  }
  
  // Array index with postfix
  let arr: i32[5] = {1, 2, 3, 4, 5};
  let i = 0;
  let val1 = arr[i++];
  let val2 = arr[i++];
  if (val1 != 1 || val2 != 2 || i != 2) {
    return 4;
  }
  
  // Multiple postfix operations
  let b = 0;
  b++;
  b++;
  let final_b = b++;
  if (final_b != 2 || b != 3) {
    return 5;
  }
  
  return 0;
}