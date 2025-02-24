import libc;

import ast;
import parse;
import sema;

import irgen;
import ir.print;

func main(argc: i32, argv: i8**) -> i32 {
  if (argc != 2) {
    puts("Usage: compile file.c");
    return -1;
  }

  let decls = parseFile(*(argv + 1));
  if (decls == null) {
    puts("Failed to parse file");
    return -1;
  }

  let semaState = initSemaState();
  decls = semaTopLevel(&semaState, decls);
  let module = genModule(decls);

  printModule(&module);

  return 0;
}
