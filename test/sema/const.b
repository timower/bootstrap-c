// RUN: %brio %s | FileCheck %s
// CHECK-NOT: alloca
// CHECK: ret i32 17
const a = 11 + 1;

const b = 1 < 11;

const c = 1 > 11;

const d = 1 <= 11;

const e = 1 >= 11;

const f = 1 == 11;

const g = 1 != 11;

const h = true && true;

const i = false && true;

const j = true || false;

const k = false || false;

const l = (1, 2);

func main() -> i32 {
  const z = a + b + c + d + e + f + g + h + i + j + k;

  return z;
}
