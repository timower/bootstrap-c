// RUN: %bootstrap %s | lli | FileCheck %s
// CHECK: Void
// CHECK-NEXT: B 99
// CHECK-NEXT: A 55
// CHECK-NEXT: B 12
// CHECK-NEXT: B 99
// CHECK-NEXT: f: 55
// CHECK-NEXT: DONE
func printf(format: const i8*, ...) -> i32;

func malloc(size: u64) -> void*;

union Foo {
  Void {
  }
  A {
    x: i32;
  }
  B {
    y: i32;
    z: i64;
  }
};

union Bar {
  A {
    w: i64;
  }
  B {
    z: i8;
  }
};

func getBar() -> Bar {
  return Bar::B{
    z = 99,
  };
}

func getFoo(f: Foo) -> Foo* {
  let ptr: Foo* = malloc(sizeof(union Foo));
  *ptr = f;
  return ptr;
}

func test(f: Foo) {
  switch (f) {
    case Foo::Void:
      printf("Void\n");
    case Foo::A as a:
      printf("A %d\n", a.x);
    case Foo::B as b:
      printf("B %d %d\n", b.y, b.z);
  }
}

func consume(b: union Bar*) {
  switch (*b) {
    case Bar::A as a:
      printf("A %d\n", a.w);
    case Bar::B as a:
      printf("B %d\n", a.z);
  }
}

func main() -> i32 {
  let foo: Foo = Foo::Void{};
  let bar: Bar = getBar();

  test(foo);
  consume(&bar);
  let baz: Bar = Bar::A{
    w = 55,
  };
  consume(&baz);
  baz = bar;

  let zero = &foo as Foo::A*;
  if (zero != NULL) {
    return 1;
  }

  let bptr = &bar as Bar::B*;
  if (bptr == NULL || bptr->z != 99) {
    return 1;
  }
  bptr->z = 12;
  consume(&bar);
  consume(&baz);

  let cptr = &getBar() as Bar::A*;
  if (cptr != NULL) {
    return 1;
  }

  let f = getFoo(Foo::A{
    x = 55,
  });

  if (f as Foo::B* != NULL) {
    return 1;
  }
  printf("f: %d\n", (f as Foo::A*)->x);

  printf("DONE\n");
  return 0;
}
