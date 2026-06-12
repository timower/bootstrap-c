// RUN: split-file %s %t
//
// RUN: not %brio %t/case.b -o %t.ll 2>&1 | FileCheck %s
// CHECK: Expected case or default
//
// RUN: not %brio %t/nobreak.b -o %t.ll 2>&1 | FileCheck %s --check-prefix CHECK2
// CHECK2: Empty case not allowed

//--- case.b
func main() -> i32 {
  let x = 0;
  switch (x) {
    func x();
  }
}

//--- nobreak.b

func main() -> i32 {
  let x = 0;
  switch (x) {
    case 1:
    case 2:
      break;
  }
}
