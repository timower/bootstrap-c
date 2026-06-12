// RUN: %brio %s | lli | FileCheck %s
// CHECK: This is
// CHECK-NEXT: a test   for '"parsing'"
// CHECK: Bar
//
extern func puts(s: i8*) -> i32;

func main() -> i32 {
  puts("This \is \n a test \t for \'\"parsing\'\"\0");
  puts("Foo\n\rBar");
  return 0;
}
