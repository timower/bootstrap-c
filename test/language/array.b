// RUN: %bootstrap %s | lli | FileCheck %s
// CHECK: Hello!
// CHECK-NEXT: baz
// CHECK-NEXT: burp
// CHECK-NEXT: baz
// CHECK-NEXT: burp
extern func puts(s: i8*) -> i32;
extern func printf(s: i8*, ...) -> i32;

let globalStr1: i8[]* = "foo";

let globalStr2: [i8] = "foo";

let test: [i8] = "foo"[:];


let array2: [i8][] = [ "baz"[:], "burp"[:] ];

func printArray() {
  let arr = array2;
  let slice = arr[:];  // TODO: allow .len on arrays.

  for (let i = 0; i < slice.len; i++) {
    puts(&slice[i][0]);
  }
}

func arrayElems() {
  let x = 12;
  let arr2 = [ x + 1, 2 ];
}

func printStr(str: [i8]) {
  printf("%d: %s\n", str.len, &str[0]);
}

func printArray2() {
  for (let i = 0; i < array2.len; i++) {
    printStr(array2[i]);
  }
}


func main() -> i32 {
  let test: [i8][2]* = &array2;
  let test2: [[i8]] = array2[:];
  let test3: [[i8]] = &array2;

  let str1: i8* = "test1";
  let str2: i8[]* = "test2";
  let str3: [i8] = "test3";

  printStr("Hello!");
  printArray();
  printArray2();
  arrayElems();

  return 0;
}
