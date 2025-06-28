// RUN: %bootstrap %s | lli | FileCheck %s
// Test evalConstant function coverage for const declarations and case expressions
// CHECK: === Const Declarations ===
// CHECK-NEXT: ADD (10 + 15): 25
// CHECK-NEXT: SUB (25 - 8): 17
// CHECK-NEXT: MUL (6 * 7): 42
// CHECK-NEXT: DIV (48 / 6): 8
// CHECK-NEXT: MOD (17 % 5): 2
// CHECK-NEXT: AND (12 & 10): 8
// CHECK-NEXT: OR (4 | 8): 12
// CHECK-NEXT: XOR (15 ^ 9): 6
// CHECK-NEXT: LSHIFT (3 << 2): 12
// CHECK-NEXT: RSHIFT (16 >> 2): 4
// CHECK-NEXT: COMPLEX1 ((5+3)*(10-6)): 32
// CHECK-NEXT: COMPLEX2 (((8<<1)+4)/(3+1)): 5
// CHECK-NEXT: COMPLEX3 ((15&7)|(4^2)): 7
// CHECK-NEXT: ENUM_VAL (ACTIVE): 1
// CHECK-NEXT: ENUM_MATH (PENDING): 2
// CHECK: === Case Expressions ===
// CHECK-NEXT: Matched addition case: 42
// CHECK-NEXT: Matched multiplication case: 24
// CHECK-NEXT: Matched modulo case: 3
// CHECK-NEXT: Matched bitwise AND case: 8
// CHECK-NEXT: Matched XOR case: 6
// CHECK-NEXT: Matched right shift case: 4
// CHECK-NEXT: Matched complex nested case: 20
// CHECK-NEXT: Status is ACTIVE
// CHECK: === Mixed Constants ===
// CHECK-NEXT: BASE * MULTIPLIER + OFFSET: 35
// CHECK-NEXT: Matched computed constant expression
// CHECK-NEXT: MASK1 & MASK2: 10
// CHECK: All const expression tests completed
extern func printf(format: i8*, ...) -> i32;

enum Status {
  INACTIVE,
  ACTIVE,
  PENDING,
};

func testConstDeclarations() {
  printf("=== Const Declarations ===\n");

  // Test basic arithmetic in const expressions
  const ADD = 10 + 15;
  const SUB = 25 - 8;
  const MUL = 6 * 7;
  const DIV = 48 / 6;
  const MOD = 17 % 5;

  printf("ADD (10 + 15): %d\n", ADD);
  printf("SUB (25 - 8): %d\n", SUB);
  printf("MUL (6 * 7): %d\n", MUL);
  printf("DIV (48 / 6): %d\n", DIV);
  printf("MOD (17 %% 5): %d\n", MOD);

  // Test bitwise operations in const expressions
  const AND_OP = 12 & 10;
  const OR_OP = 4 | 8;
  const XOR_OP = 15 ^ 9;
  const LSHIFT = 3 << 2;
  const RSHIFT = 16 >> 2;

  printf("AND (12 & 10): %d\n", AND_OP);
  printf("OR (4 | 8): %d\n", OR_OP);
  printf("XOR (15 ^ 9): %d\n", XOR_OP);
  printf("LSHIFT (3 << 2): %d\n", LSHIFT);
  printf("RSHIFT (16 >> 2): %d\n", RSHIFT);

  // Test nested expressions
  const COMPLEX1 = (5 + 3) * (10 - 6);
  const COMPLEX2 = ((8 << 1) + 4) / (3 + 1);
  const COMPLEX3 = (15 & 7) | (4 ^ 2);

  printf("COMPLEX1 ((5+3)*(10-6)): %d\n", COMPLEX1);
  printf("COMPLEX2 (((8<<1)+4)/(3+1)): %d\n", COMPLEX2);
  printf("COMPLEX3 ((15&7)|(4^2)): %d\n", COMPLEX3);

  // Test enum values as constants
  const ENUM_VAL = Status::ACTIVE;
  const ENUM_MATH = Status::PENDING;

  printf("ENUM_VAL (ACTIVE): %d\n", ENUM_VAL);
  printf("ENUM_MATH (PENDING): %d\n", ENUM_MATH);
}

func testCaseExpressions() {
  printf("=== Case Expressions ===\n");

  // Test arithmetic in case expressions
  let x = 42;
  switch (x) {
    case (30 + 12):
      printf("Matched addition case: 42\n");
      break;
    case (50 - 7):
      printf("Should not match subtraction\n");
      break;
    default:
      printf("No arithmetic match\n");
  }

  // Test multiplication and division
  let y = 24;
  switch (y) {
    case (4 * 6):
      printf("Matched multiplication case: 24\n");
      break;
    case (50 / 2):
      printf("Should not match division\n");
      break;
    default:
      printf("No mul/div match\n");
  }

  // Test modulo
  let z = 3;
  switch (z) {
    case (19 % 8):
      printf("Matched modulo case: 3\n");
      break;
    case (17 % 4):
      printf("Should not match other modulo\n");
      break;
    default:
      printf("No modulo match\n");
  }

  // Test bitwise operations in cases
  let a = 8;
  switch (a) {
    case (12 & 10):
      printf("Matched bitwise AND case: 8\n");
      break;
    case (6 | 2):
      printf("Should not match bitwise OR\n");
      break;
    default:
      printf("No bitwise match\n");
  }

  // Test XOR and shifts
  let b = 6;
  switch (b) {
    case (15 ^ 9):
      printf("Matched XOR case: 6\n");
      break;
    case (2 << 1):
      printf("Should not match left shift\n");
      break;
    default:
      printf("No XOR/shift match\n");
  }

  // Test right shift
  let c = 4;
  switch (c) {
    case (16 >> 2):
      printf("Matched right shift case: 4\n");
      break;
    case (10 >> 1):
      printf("Should not match other right shift\n");
      break;
    default:
      printf("No right shift match\n");
  }

  // Test complex nested expressions in cases
  let d = 20;
  switch (d) {
    case ((2 + 3) * (8 / 2)):
      printf("Matched complex nested case: 20\n");
      break;
    case ((10 | 5) + (6 & 3)):
      printf("Should not match complex bitwise\n");
      break;
    default:
      printf("No complex match\n");
  }

  // Test enum values in cases
  let status = Status::ACTIVE;
  switch (status) {
    case Status::INACTIVE:
      printf("Status is INACTIVE\n");
      break;
    case Status::ACTIVE:
      printf("Status is ACTIVE\n");
      break;
    case Status::PENDING:
      printf("Status is PENDING\n");
      break;
  }
}

func testMixedConstants() {
  printf("=== Mixed Constants ===\n");

  // Test constants used in arithmetic
  const BASE = 10;
  const MULTIPLIER = 3;
  const OFFSET = 5;

  let result = BASE * MULTIPLIER + OFFSET;
  printf("BASE * MULTIPLIER + OFFSET: %d\n", result);

  // Use computed constants in switch
  switch (result) {
    case (BASE * MULTIPLIER + OFFSET):
      printf("Matched computed constant expression\n");
      break;
    default:
      printf("No constant match\n");
  }

  // Test constants with bitwise operations
  const MASK1 = 0b1111;
  const MASK2 = 0b1010;
  const COMBINED = MASK1 & MASK2;

  printf("MASK1 & MASK2: %d\n", COMBINED);
}

func main() -> i32 {
  testConstDeclarations();
  testCaseExpressions();
  testMixedConstants();

  printf("All const expression tests completed\n");
  return 0;
}

