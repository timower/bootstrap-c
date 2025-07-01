// RUN: %bootstrap %s | %check-exit-code 5 lli
func returnInt() -> i32 {
  return 42;
}

func returnTypeof() -> typeof(returnInt()) {
  return 2;
}

func paramTypeof(x: typeof(returnInt())) -> i32 {
  return 5;
}

struct Test {
  x: typeof(returnTypeof());
  y: typeof(&returnInt);
};

func main() -> i32 {
  let x: typeof(Test {}.x) = 0;
  x = 2;

  let y = Test {
    x = 0,
    y = &returnTypeof,
  };

  x = (y.y)();

  return returnTypeof() - x + paramTypeof(x);
}
