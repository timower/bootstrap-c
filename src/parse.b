import parse.internal;

func parse(state: ParseState*) -> DeclAST* {
  state->line = 1;

  state->jmpBuf = newJmpBuf();

  // TODO: defer free(state->jmpBuf);
  if (setjmp(state->jmpBuf) != 0) {
    free(state->jmpBuf);
    return null;
  }

  let res = parseTopLevel(state);
  free(state->jmpBuf);
  return res;
}

func parseFile(allocator: Allocator*, name: const i8*) -> DeclAST* {
  let buf = readFile(allocator, name);
  let state = ParseState {
    buf = buf,
    fileName = name,
    concrete = false,
  };
  return parse(&state);
}
