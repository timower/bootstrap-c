// RUN: split-file %s %t
// RUN: chmod -r %t/foo.b
// RUN: not %brio %t/main.b 2>&1 | FileCheck %s
//
// CHECK: open failed
// CHECK: Failed to import file
//
//--- main.b
import foo;


//--- foo.b
func bar() {

}
