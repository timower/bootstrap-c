// RUN: %bootstrap %s | lli | FileCheck %s
// CHECK: Void
// CHECK-NEXT: B 99
// CHECK-NEXT: A 55
// CHECK-NEXT: B 12
// CHECK-NEXT: B 99
// CHECK-NEXT: f: 55
// CHECK-NEXT: B 65
// CHECK-NEXT: B 66
// CHECK-NEXT: DONE
extern func printf(format: i8*, ...) -> i32;
extern func malloc(size: u64) -> void*;

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
  return Bar::B {
    z = 'c',
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
  let foo: Foo = Foo::Void {};
  let bar: Bar = getBar();

  test(foo);
  consume(&bar);
  let baz: Bar = Bar::A {
    w = 55,
  };
  consume(&baz);
  baz = bar;

  let zero = &foo as Foo::A*;
  if (zero != null) {
    return 1;
  }

  let bptr = &bar as Bar::B*;
  if (bptr == null || bptr->z != 'c') {
    return 2;
  }
  bptr->z = 12;
  consume(&bar);
  consume(&baz);

  let cptr = &getBar() as Bar::A*;
  if (cptr != null) {
    return 3;
  }

  let f = getFoo(Foo::A {
    x = 55,
  });

  if (f as Foo::B* != null) {
    return 4;
  }
  printf("f: %d\n", (f as Foo::A*)->x);

  if (let ptr = &bar as Bar::B*) {
    ptr->z = 'A';
  }
  consume(&bar);

  if (let ptr = bar as Bar::B*) {
    ptr->z = 'B';
  }
  consume(&bar);

  printf("DONE\n");
  return 0;
}
