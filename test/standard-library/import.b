// RUN: rm -rf %t && split-file %s %t
// RUN: cd %t && %brio test.b
//
// RUN: mkdir -p %t/bin
// RUN: cp %raw_brio %t/bin/brio
//
// RUN: cd %t && not timeout 2 ./bin/brio test.b 2>&1 | grep "Can't find stdlib"
// RUN: cd %t && timeout 2 ./bin/brio -stdlib %root_dir test.b
//
// RUN: mkdir -p %t/lib/brio
// RUN: ln -sf %root_dir/stdlib %t/lib/brio/
// RUN: cd %t && timeout 2 ./bin/brio test.b
//
// RUN: not %brio %t/fail1.b 2>&1 | grep "Couldn't find stdlib file"
// RUN: not %brio %t/fail2.b 2>&1 | grep "Couldn't find stdlib file"
//
//--- test.b
import stdlib.libc;

func main() -> i32 {
  puts("Hello!");
  return 0;
}


//--- fail1.b
import stdlib.does.not.exist;


//--- fail2.b
import stdlib;
