// RUN: %bootstrap %s | lli
// Test constants and compile-time evaluation

const GLOBAL_INT = 42;

const HEX_VALUE = 0xFF;
const OCTAL_VALUE = 0o77;  
const BINARY_VALUE = 0b1111;

func main() -> i32 {
  const LOCAL_CONST = 100;
  
  let value1 = GLOBAL_INT;
  let value2 = HEX_VALUE;
  let value3 = OCTAL_VALUE;
  let value4 = BINARY_VALUE;
  let value5 = LOCAL_CONST;
  
  return 0;
}