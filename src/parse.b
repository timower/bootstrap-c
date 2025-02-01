import parse.internal;

func parseBufOpts(name: i8*, buf: Buf, options: ParseOptions) -> DeclAST* {
  // clang-format off
  let parseState = ParseState {
    fileName = name,
    current = buf.mem,
    start = buf.mem,
    end = buf.mem + buf.size,
    line = 1,
    lineStart = buf.mem,
    options = options,
  };

  // clang-format on
  return parseTopLevel(&parseState);
}

func parseFile(name: const i8*) -> DeclAST* {
  let buf = readFile(name);
  if (buf.mem == null) {
    return null;
  }
  return parseBufOpts(name, buf, ParseOptions {});
}
