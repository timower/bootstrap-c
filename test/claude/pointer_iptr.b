// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli  
// Test pointer-sized integer type (getIPtr function)

func main() -> i32 {
  // Test pointer subtraction with char pointers (since only char pointer subtract supported)
  let str = "hello world test";
  let ptr1 = &str[10];
  let ptr2 = &str[2];
  
  // Pointer subtraction should use iptr type
  let diff = ptr1 - ptr2;
  if (diff != 8) {
    return 1;
  }
  
  // Test pointer arithmetic with char pointers
  let base_ptr = &str[10];
  let offset_ptr = &str[5];
  let offset_diff = base_ptr - offset_ptr;
  if (offset_diff != 5) {
    return 2;
  }
  
  // Test medium char array pointer arithmetic  
  let medium_str = "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789";
  let start_ptr = &medium_str[0];
  let end_ptr = &medium_str[99];
  let total_diff = end_ptr - start_ptr;
  if (total_diff != 99) {
    return 3;
  }
  
  // Test pointer comparison using char pointer arithmetic
  let ptr_a = &str[3];
  let ptr_b = &str[7];
  let comparison_diff = ptr_b - ptr_a;
  if (comparison_diff <= 0) {
    return 4;
  }
  
  return 0;
}

// CHECK: define i32 @main()
// CHECK: getelementptr inbounds i8, ptr {{.*}}, i64 10
// CHECK: getelementptr inbounds i8, ptr {{.*}}, i64 2
// CHECK: ptrtoint ptr {{.*}} to i64
// CHECK: ptrtoint ptr {{.*}} to i64
// CHECK: sub i64 {{.*}}, {{.*}}
// CHECK: icmp ne i64 {{.*}}, 8