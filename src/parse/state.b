import ast;
import ast.print;

struct ParseOptions {
  // If set to true, build a concere syntax tree,
  // preserving parens.
  concrete: bool;
}

const source_slab_size = 512;

struct ParseState {
  // Any parse options the parser was constructed with.
  options: ParseOptions;

  // [start, end[ contains the current data buffer.
  start: i8*;
  end: i8*;

  // Pointer in [start, end[ where we're currently parsing.
  current: i8*;

  // Currently parsed token.
  curToken: Token;

  // current file name.
  fileName: i8*;
  line: i32;
  lineStart: i8*;

  currentSlab: SourceLoc*;
  slabFree: i32;

  // Any comments that should be taken up by the next node.
  // Only parsed if concrete is true.
  comments: Comment*;
  lastComment: Comment*;
}

func getLocation(state: ParseState*) -> SourceLoc* {
  if (state->slabFree == 0) {
    state->currentSlab = calloc(source_slab_size, sizeof(SourceLoc)) as SourceLoc*;
    state->slabFree = source_slab_size;
  }

  let result = state->currentSlab;
  state->currentSlab++;
  state->slabFree--;

  let offset = state->current;

  // TODO: remove once getLocation is only used in parse/token.b
  if (state->curToken.kind != TokenKind::TOK_EOF) {
    offset = state->curToken.location->data;
  }
  result->data = offset;
  result->column = (offset - state->lineStart) as i32 + 1;
  result->line = state->line;
  result->fileName = state->fileName;
  return result;
}


// Pops any comments on the given line from state.
func getLineComments(state: ParseState*, line: i32) -> Comment* {
  let firstComment = state->comments;
  if (firstComment == null || firstComment->location->line > line) {
    return null;
  }

  let lastComment = firstComment;
  while (lastComment->next != null
      && lastComment->next->location->line <= line) {
    lastComment = lastComment->next;
  }

  state->comments = lastComment->next;
  if (lastComment->next == null) {
    state->lastComment = null;
  }
  lastComment->next = null;

  return firstComment;
}

func appendComments(list: Comment*, other: Comment*) -> Comment* {
  if (list == null) {
    return other;
  }

  let lastComment = list;
  while (lastComment->next != null) {
    lastComment = lastComment->next;
  }
  lastComment->next = other;
  return list;
}

func failParseArg(state: ParseState*, msg: const i8*, arg: const i8*) {
  let location = getLocation(state);
  fprintf(getStderr(), "%s:%d:%d: ", state->fileName, location->line, location->column);

  fprintf(getStderr(), ": %s%s\n", msg, arg);
  exit(1);
}

func failParse(state: ParseState*, msg: const i8*) {
  failParseArg(state, msg, "");
}

func match(state: ParseState*, tok: TokenKind) -> bool {
  return state->curToken.kind == tok;
}

func expect(state: ParseState*, tok: TokenKind) {
  if (!match(state, tok)) {
    failParseArg(state, "Expected: ", tokens[(tok as i32)]);
  }
}

func newLocDecl(state: ParseState*, kind: DeclKind) -> DeclAST* {
  let res = newDecl(kind);
  res->location = getLocation(state);

  if (state->options.concrete) {
    res->comments = state->comments;
    state->comments = null;
    state->lastComment = null;
  }

  return res;
}

func newLocExpr(loc: SourceLoc*, kind: ExprKind) -> ExprAST* {
  let res = newExpr(kind);
  res->location = loc;
  return res;
}

func newCurLocExpr(state: ParseState*, kind: ExprKind) -> ExprAST* {
  let res = newExpr(kind);
  res->location = getLocation(state);
  return res;
}

func newLocStmt(state: ParseState*, kind: StmtKind) -> StmtAST* {
  let res = newStmt(kind);
  res->location = getLocation(state);

  if (state->options.concrete) {
    res->comments = state->comments;
    state->comments = null;
    state->lastComment = null;
  }

  return res;
}
