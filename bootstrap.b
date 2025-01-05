import libc;

import ast;
import parse;
import sema;
import emit_llvm;

struct DeclAST *parseFile(const i8 *name) {
  i32 fd = open(name, 0); //  O_RDONLY
  if (fd == -1) {
    puts("open failed!");
    return NULL;
  }

  i64 size = lseek(fd, 0, 2); //  SEEK_END
  if (size == -1) {
    puts("seek failed!");
    return NULL;
  }

  if (lseek(fd, 0, 0) == -1) { // SEEK_SET
    puts("seek failed!");
    return NULL;
  }

  i8 *fileMem = malloc(size as u64);

  i64 off = 0;
  while (off != size) {
    i64 r = read(fd, fileMem + off, (size - off) as u64);
    if (r == -1) {
      puts("read failed!");
      return NULL;
    }
    off += r;
  }

  struct ParseState parseState = {0};
  parseState.current = parseState.start = fileMem;
  parseState.end = fileMem + size;

  return parseTopLevel(&parseState);
}

i32 main(i32 argc, i8 **argv) {
  if (argc != 2) {
    puts("Usage: compile file.c");
    return -1;
  }

  struct DeclAST *decls = parseFile(argv[1]);
  if (decls == NULL) {
    puts("Failed to parse file");
    return -1;
  }

  struct SemaState semaState = initState();
  decls = semaTopLevel(&semaState, decls);

  struct EmitState emitState = {0};
  emitTopLevel(&emitState, decls);

  return 0;
}
