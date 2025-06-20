// RUN: split-file %s %t
//
// RUN: not %bootstrap %t/empty_at_end.b 2>&1 | grep "Empty case not allowed"
// RUN: %bootstrap %t/union_comma.b | lli
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
