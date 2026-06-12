// RUN: not %brio %s 2>&1 | grep 'Variable redef'
func foo() {
  let x = 1;
  let x = 2;
}

