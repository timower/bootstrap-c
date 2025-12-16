// RUN: %bootstrap %s -o %t.ll
// RUN: opt -S %t.ll
// RUN: lli %t.ll | FileCheck %s
//
// CHECK: len: 12
// CHECK-NEXT: 0: 1
// CHECK-NEXT: 1: 2
// CHECK-NEXT: 2: 3
// CHECK-NEXT: 3: 4
// CHECK-NEXT: 4: 5
// CHECK-NEXT: 5: 6
// CHECK-NEXT: 6: 7
// CHECK-NEXT: 7: 8
// CHECK-NEXT: 8: 9
// CHECK-NEXT: 9: 10
// CHECK-NEXT: 10: 11
// CHECK-NEXT: 11: 1
//
extern func malloc(size: iptr) -> void*;
extern func printf(s: i8*, ...) -> i32;

let array: i32[] = [ 1, 2, 3 ];

func newSlice(size: i32) -> [i32] {
  let ptr = malloc((size * sizeof(i32)) as iptr) as i32*;
  return ptr[:size];
}


func test1() -> i32 {
  let slice: [i32] = array[:];
  return slice[1] - 2;
}

func useSlice[T](slice: [T]) -> [T] {
  printf("len: %d\n", slice.len);
  for (let i = 0; i < slice.len; i++) {
    printf("%d: %d\n", i, slice[i]);
  }

  return slice[1:];
}

func test2() -> i32 {
  let slice = newSlice(12);

  if (slice.len != 12) {
    return 1;
  }

  for (let i = 0; i < slice.len; i++) {
    slice[i] = i + 1;
  }

  let res = useSlice:[i32](slice);
  if (res.len != 11) {
    return 1;
  }

  let sub = res[1:2];

  return sub[0] - 3;
}

func main() -> i32 {
  return test1() + test2();
}
