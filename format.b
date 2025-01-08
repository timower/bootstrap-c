import libc;

import ast;
import parse;

func main(argc : i32, argv : i8 **) -> i32 {
  if (argc != 2) {
    puts("Usage: compile file.c");
    return -1;
  }

  let parseOpts = ParseOptions{concrete = 1};
  let decls = parseFileOpts(*(argv + 1), parseOpts);

  printTopLevel(decls);

  return 0;
}
