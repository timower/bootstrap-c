// RUN: %bootstrap %s -o %t.ll
// RUN: FileCheck %s < %t.ll
// RUN: lli %t.ll
// Test tagged unions

// CHECK: %union.Result = type
// CHECK: alloca %union.Result
// CHECK: getelementptr inbounds %union.Result
// CHECK: switch i32

union Result {
  Success {
    value: i32;
  }
  Error {
    msg: i8*;
  }
};

func main() -> i32 {
  let success: Result = Result::Success {
    value = 42,
  };
  
  switch (success) {
    case Result::Success as s:
      return 0;
    case Result::Error as e:
      return 1;
  }
}