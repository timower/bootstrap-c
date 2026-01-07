// RUN: split-file %s %t
//
// RUN: not %bootstrap %t/empty_at_end.b 2>&1 | grep "Empty case not allowed"
// RUN: %bootstrap %t/union_comma.b | lli
// RUN: not %bootstrap %t/unknown_type.b 2>&1 | grep "Couldn't find type"
// RUN: not %bootstrap %t/unknown_type2.b 2>&1 | grep "Unknown union"
// RUN: not %bootstrap %t/unknown_type3.b 2>&1 | grep "Expected union type"
// RUN: not %bootstrap %t/unknown_tag1.b 2>&1 | grep "Cannot find tag"
// RUN: not %bootstrap %t/unknown_tag2.b 2>&1 | grep "Unkown tag in union"
// RUN: not %bootstrap %t/unknown_field.b 2>&1 | grep "Cannot find field"
// RUN: not %bootstrap %t/wrong_type1.b 2>&1 | grep "Expected union or enum"
// RUN: not %bootstrap %t/wrong_type2.b 2>&1 | grep "case expr must match switch type"
// RUN: not %bootstrap %t/wrong_type3.b 2>&1 | grep "Switch expr must be integer"
// RUN: not %bootstrap %t/no_scope.b 2>&1 | grep "Expected :: expression"
// RUN: not %bootstrap %t/not_exhaustive.b 2>&1 | grep "Switch is not exhaustive"
// RUN: not %bootstrap %t/no_decl.b 2>&1 | grep "Couldn't find enum decl"
//
//--- empty_at_end.b
func main() -> i32 {
  let a = 0;
  switch (a) {
    case 1:
      return 0;

    case 2:

  }
  return 1;
}


//--- union_comma.b
union Foo {
  A {}
  B {}
  C {}
};

func main() -> i32 {
  let x: Foo = Foo::A {};
  switch (x) {
    case Foo::A, Foo::B:
      return 0;
    case Foo::C as c:
      return 1;
  }
  return 1;
}

//--- unknown_type.b
func main() -> i32 {
  let x = 1;
  switch (x) {
    case Foo::A:
      return 0;
  }
  return 1;
}

//--- unknown_type2.b
func main() -> i32 {
  let x = 1;
  switch (x) {
    case Foo::C as c:
      return 1;
  }
  return 1;
}

//--- unknown_type3.b
struct Foo{};
func main() -> i32 {
  let x = 1;
  switch (x) {
    case Foo::C as c:
      return 1;
  }
  return 1;
}

//--- unknown_tag1.b
union Foo {};
func main() -> i32 {
  let x = 1;
  switch (x) {
    case Foo::A:
      return 0;
  }
  return 1;
}

//--- unknown_tag2.b
union Foo {};
func main() -> i32 {
  let x = 1;
  switch (x) {
    case Foo::C as c:
      return 1;
  }
  return 1;
}

//--- unknown_field.b
enum Foo {};
func main() -> i32 {
  let x = 1;
  switch (x) {
    case Foo::A:
      return 0;
  }
  return 1;
}

//--- wrong_type1.b
struct Foo {};
func main() -> i32 {
  let x = 1;
  switch (x) {
    case Foo::A:
      return 0;
  }
  return 1;
}

//--- wrong_type2.b
enum Foo{A,};
func main() -> i32 {
  let x = Foo::A;
  switch (x) {
    case 12:
      return 0;
  }
  return 1;
}

//--- wrong_type3.b
struct Foo{};
func main() -> i32 {
  let x = Foo{};
  switch (x) {
    case 12:
      return 0;
  }
  return 1;
}

//--- no_scope.b
struct Foo {};
func main() -> i32 {
  let x = 1;
  switch (x) {
    case a as a:
      return 0;
  }
  return 1;
}


//--- not_exhaustive.b
enum Foo { A,};
func foo(x: Foo) -> i32 {
  switch (x) {
  }
  return 1;
}
//--- no_decl.b
func foo(x: enum Foo) -> i32 {
  switch (x) {
  }
  return 1;
}
