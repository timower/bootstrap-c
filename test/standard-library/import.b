// RUN: rm -rf %t && split-file %s %t
// RUN: cd %t && %bootstrap test.b
//
// RUN: mkdir -p %t/bin
// RUN: cp %raw_bootstrap %t/bin/bootstrap
//
// RUN: cd %t && not timeout 2 ./bin/bootstrap test.b 2>&1 | grep "Can't find stdlib"
// RUN: cd %t && timeout 2 ./bin/bootstrap -stdlib %root_dir test.b
//
// RUN: mkdir -p %t/lib/bootstrap
// RUN: ln -sf %root_dir/stdlib %t/lib/bootstrap/
// RUN: cd %t && timeout 2 ./bin/bootstrap test.b
//
// RUN: not %bootstrap %t/fail1.b 2>&1 | grep "Couldn't find stdlib file"
// RUN: not %bootstrap %t/fail2.b 2>&1 | grep "Couldn't find stdlib file"
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
