// RUN: %bootstrap %s | FileCheck %s
// RUN: %bootstrap %s | lli
struct Foo {
  x: i32;
  y: i32;
};

func getFoo() -> Foo {
  return Foo{
    x = 1,
    y = 2,
  };
}

func main() -> i32 {
  return getFoo().y - 2 * getFoo().x;
}

// CHECK: define i32 @main()
// CHECK: define {{.*}} @getFoo()
// CHECK: call {{.*}} @getFoo()
// CHECK: extractvalue {{.*}} 1
// CHECK: call {{.*}} @getFoo()
// CHECK: extractvalue {{.*}} 0
