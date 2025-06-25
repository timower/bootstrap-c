// RUN: split-file %s %t
// RUN: %bootstrap %t/main.b | FileCheck %s
// RUN: %bootstrap %t/main.b | lli
//
// Test multi-file imports and modules

//--- main.b
import math.calculator;
import utils.helpers;

func main() -> i32 {
  let result = add(10, 5);
  let doubled = double(result);
  let greeting = getGreeting();
  
  return 0;
}

//--- math/calculator.b
func add(a: i32, b: i32) -> i32 {
  return a + b;
}

func subtract(a: i32, b: i32) -> i32 {
  return a - b;
}

func multiply(a: i32, b: i32) -> i32 {
  return a * b;
}

//--- utils/helpers.b
func double(x: i32) -> i32 {
  return x * 2;
}

func triple(x: i32) -> i32 {
  return x * 3;
}

func getGreeting() -> i8* {
  return "Hello from utils!";
}

// CHECK: define i32 @main()
// CHECK: define i32 @add(i32 %0, i32 %1)
// CHECK: define i32 @double(i32 %0)
// CHECK: define ptr @getGreeting()
// CHECK: call i32 @add(i32 10, i32 5)
// CHECK: call i32 @double(i32 %{{[0-9]+}})
// CHECK: call ptr @getGreeting()