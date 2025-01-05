import libc;

import ast;
import parse;
import sema;
import emit_llvm;

func main(argc : i32, argv : i8 **) -> i32 {
  if (argc != 2) {
    puts("Usage: compile file.c");
    return -1;
  }

  let decls = parseFile(*(argv + 1));
  if (decls == NULL) {
    puts("Failed to parse file");
    return -1;
  }

  let semaState = initSemaState();
  decls = semaTopLevel(&semaState, decls);

  // for (let decl = decls; decl != NULL; decl = decl->next) {
  //   printDecl(decl);
  // }

  emitTopLevel(decls);

  return 0;
}
