// RUN: %bootstrap %s -o %t.ll
// RUN: FileCheck %s < %t.ll
// RUN: lli %t.ll
// Test integer casting (semaIntCast function)

// CHECK: trunc i32
// CHECK: icmp ne
// CHECK: ret i32
func testCast(val: i32) -> i8 {
  return val as i8;
}

func main() -> i32 {
  // Test explicit casting with function return
  let result1: i8 = testCast(300);
  if (result1 != 44) {
    // 300 & 0xFF = 44
    return 1;
  }

  let test = result1 as u8;

  let foo = result1 as i32;
  let bar = test as u32;

  // Test casting with different sizes
  let big_val: i32 = 1000;
  let small_val: i8 = big_val as i8;
  if (small_val != -24) {
    // 1000 & 0xFF = 232, as i8 = -24
    return 2;
  }

  // Test casting preserves small values
  let small_input: i32 = 50;
  let preserved: i8 = small_input as i8;
  if (preserved != 50) {
    return 3;
  }

  // Test casting with negative values
  let negative: i32 = -1;
  let cast_negative = negative as u8;
  if (cast_negative != -1) {
    return 4;
  }

  return 0;
}
