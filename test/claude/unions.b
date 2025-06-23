// RUN: %bootstrap %s | lli
// Test tagged unions

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