// RUN: echo 'func main(){return 0;}' > %t.b
// RUN: %bootstrap -format -i %t.b
// RUN: cat %t.b | FileCheck %s

// CHECK: func main() {
// CHECK-NEXT:     return 0;
// CHECK-NEXT: }