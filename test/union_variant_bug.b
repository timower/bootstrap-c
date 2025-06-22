// RUN: not %bootstrap %s 2>&1 | grep "Expected enum type for scope expr"
union Foo {
  A {}
  B {}
};

func main() -> i32 {
  let foo = Foo::A;
  return 0;
}
