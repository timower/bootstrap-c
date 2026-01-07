import parse.internal;

func parseBufOpts(name: i8*, buf: [i8], options: ParseOptions) -> DeclAST* {
  // clang-format off
  let parseState = ParseState {
    options = options,
    buf = buf,
    fileName = name,
    line = 1,
    jmpBuf = newJmpBuf(),
  };

  if (setjmp(parseState.jmpBuf) != 0) {
    return null;
  }

  // clang-format on
  return parseTopLevel(&parseState);
}

func parseFile(name: const i8*) -> DeclAST* {
  let buf = readFile(name);
  if (&buf[0] == null) {
    return null;
  }
  return parseBufOpts(name, buf, ParseOptions {});
}
