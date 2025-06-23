// RUN: %bootstrap %s -target posix | lli
// RUN: %bootstrap %s -target darwin | lli  
// REQUIRES: system-darwin
// Test cross-platform compilation targets

func main() -> i32 {
  let platform_value = 42;
  
  // Test basic functionality across targets
  let a = 10;
  let b = 20;
  let sum = a + b;
  
  if (sum == 30) {
    return 0;
  }
  
  return 1;
}