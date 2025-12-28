// RUN: %bootstrap %s | lli | FileCheck %s
// CHECK: Hello!
// CHECK-NEXT: foo
// CHECK-NEXT: bar
extern func puts(s: i8*) -> i32;

let array: i8*[] = [ "foo", "bars" ];

let globalStr1: i8[]* = "foo";

let globalStr2: [i8] = "foo";

let test: [i8] = "foo"[:];


let array2: [i8][] = [ "baz"[:], "burp"[:] ];

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

func printStr(str: [i8]) {
  puts(&str[0]);
}

func printArray2() {
  for (let i = 0; i < array2.len; i++) {
    printStr(array2[i]);
  }
}


func main() -> i32 {
  let test: i8*[2]* = &array;
  let test2: [i8*] = array[:];
  let test3: [i8*] = &array;

  let str1: i8* = "test1";
  let str2: i8[]* = "test2";
  let str3: [i8] = "test3";

  printStr("Hello!");
  printArray();
  arrayElems();

  return 0;
}
