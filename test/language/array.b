// RUN: %bootstrap %s | lli | FileCheck %s
// CHECK: foo
// CHECK-NEXT: bar
extern func puts(s: i8*) -> i32;

let array = [ "foo", "bars" ];

func printArray() {
  let arr = array;
  let slice = arr[:];  // TODO: allow .len on arrays.

  for (let i = 0; i < slice.len; i++) {
    puts(slice[i]);
  }
}

func arrayElems() {
  let x = 12;
  let arr2 = [ x + 1, 2 ];
}


func main() -> i32 {
  printArray();
  arrayElems();

  return 0;
}
