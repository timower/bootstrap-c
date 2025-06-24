// Test case for backwards linear scan register allocation
// This test verifies that the register allocator efficiently reuses registers
// instead of running out of available registers
// RUN: %bootstrap -emit-asm %s | FileCheck %s

func main() -> i32 {
  // Chain arithmetic operations to create many SSA values
  // With proper register allocation, this should reuse registers efficiently
  return 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10;
}

// CHECK: {{_?}}main:
// With backwards linear scan allocation, registers should be reused efficiently
// Should only use w1 and w2, not higher numbered registers
// CHECK-NOT: w3
// CHECK-NOT: w4
// CHECK-NOT: w5
