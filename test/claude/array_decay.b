// RUN: %bootstrap %s -o %t.ll
// RUN: FileCheck %s < %t.ll
// RUN: lli %t.ll
// Test array decay (doDecay function)

// CHECK: alloca [5 x i32]
// CHECK: getelementptr
// CHECK: load i32
// CHECK: icmp ne i32
// CHECK: ret i32

func main() -> i32 {
  // Test basic array access
  let arr: i32[5] = {10, 20, 30, 40, 50};
  
  // Test array indexing 
  let first = arr[0];
  if (first != 10) {
    return 1;
  }
  
  let second = arr[1];
  if (second != 20) {
    return 2;
  }
  
  // Test array address taking
  let arr_ptr = &arr[0];
  if (*arr_ptr != 10) {
    return 3;
  }
  
  // Test pointer arithmetic with char arrays (since only char pointer subtract supported)
  let str = "hello";
  let char_ptr2 = &str[2];
  let char_ptr0 = &str[0];
  let char_diff = char_ptr2 - char_ptr0;
  if (char_diff != 2) {
    return 4;
  }
  
  return 0;
}