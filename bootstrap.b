import libc;

import ast;
import parse;
import sema;
import emit_llvm;

func parseFile(name : const i8 *) -> DeclAST * {
  let fd = open(name, 0); //  O_RDONLY
  if (fd == -1) {
    puts("open failed!");
    return NULL;
  }

  let size = lseek(fd, 0, 2); //  SEEK_END
  if (size == -1) {
    puts("seek failed!");
    return NULL;
  }

  if (lseek(fd, 0, 0) == -1) { // SEEK_SET
    puts("seek failed!");
    return NULL;
  }

  let fileMem : i8 * = malloc(size as u64);

  let off : i64 = 0;
  while (off != size) {
    let r = read(fd, fileMem + off, (size - off) as u64);
    if (r == -1) {
      puts("read failed!");
      return NULL;
    }
    off += r;
  }

  let parseState : ParseState = {0};
  parseState.current = parseState.start = fileMem;
  parseState.end = fileMem + size;

  return parseTopLevel(&parseState);
}

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

  let emitState : EmitState = {0};
  emitTopLevel(&emitState, decls);

  return 0;
}
