// Test for bug where variants from different unions with same name can be cast to each other
// RUN: not %bootstrap -sema %s 2>&1 | FileCheck %s

union FirstUnion {
  Common {
    value: i32;
  }
  First {
    data: i64;
  }
}

union SecondUnion {
  Common {
    value: i32;
  }
  Second {
    info: i8*;
  }
}

func main() -> i32 {
  let first: FirstUnion = FirstUnion::Common {
    value = 42,
  };

  // This should fail - cannot cast FirstUnion to SecondUnion::Common*
  // even though they have the same variant name
  let second = first as SecondUnion::Common*;

  // CHECK: sema error: Cannot cast union to pointer of variant from different union
  return 0;
}