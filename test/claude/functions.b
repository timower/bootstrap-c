// RUN: %bootstrap %s | lli
// Test function parameters, return types, and calling conventions

func noParams() -> i32 {
  return 42;
}

func singleParam(x: i32) -> i32 {
  return x * 2;
}

func multipleParams(a: i32, b: i32, c: i32) -> i32 {
  return a + b + c;
}

func differentTypes(i: i32, b: bool, p: i32*) -> i32 {
  if (b) {
    return i + *p;
  }
  return i;
}

func voidReturn(x: i32) -> void {
  let temp = x + 1;
}

func passByValue(x: i32) -> i32 {
  x = x + 10;
  return x;
}

func passByPointer(x: i32*) -> void {
  *x = *x + 10;
}

func recursive(n: i32) -> i32 {
  if (n <= 1) {
    return 1;
  }
  return n * recursive(n - 1);
}

func callOtherFunction() -> i32 {
  let result = singleParam(5);
  return result + 3;
}

func main() -> i32 {
  let a = noParams();  
  let b = singleParam(10);
  let c = multipleParams(1, 2, 3);
  
  let value = 5;
  let d = differentTypes(10, true, &value);
  
  voidReturn(123);
  
  let original = 20;
  let e = passByValue(original);
  
  let mutable = 30;
  passByPointer(&mutable);
  
  let fact = recursive(5);
  let call_result = callOtherFunction();
  
  return 0;
}