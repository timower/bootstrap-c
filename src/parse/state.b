import ast;
import ast.print;

import stdlib.libc;
import alloc;


struct ParseState {
  // If set to true, build a concere syntax tree, preserving parens.
  concrete: bool;

  // [start, end[ contains the current data buffer.
  buf: [i8];

  astAlloc: Allocator*;

  // current file name.
  fileName: i8*;

  // Pointer in [start, end[ where we're currently parsing.
  current: i32;
  lineStart: i32;

  // Currently parsed token.
  curToken: Token;

  line: i32;

  // Any comments that should be taken up by the next node.
  // Only parsed if concrete is true.
  comments: Comment*;
  lastComment: Comment*;

  jmpBuf: JmpBuf*;

  // Limit the depth of primary expressions, to prevent stack overflow
  depth: i32;
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

  // Doesn't seem to be needed:
  // if ( lastComment->next == null) {
  state->lastComment = null;
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
  let location = state->curToken.location;
  printLoc(location);

  fprintf(getStderr(), ": %s%s\n", msg, arg);
  longjmp(state->jmpBuf, 1);
}

func failParse(state: ParseState*, msg: const i8*) {
  failParseArg(state, msg, "");
}

func match(state: ParseState*, tok: TokenKind) -> bool {
  return state->curToken.kind == tok;
}

func expect(state: ParseState*, tok: TokenKind) {
  if (!match(state, tok)) {
    failParseArg(state, "Expected: ", &tokens[(tok as i32)][0]);
  }
}

func newLocDecl(state: ParseState*, kind: DeclKind) -> DeclAST* {
  let res = newDecl(state->astAlloc, kind);
  res->location = state->curToken.location;

  // opt: Only parse comments if concrete is enabled.
  if (state->concrete) {
    res->comments = state->comments;
    state->comments = null;
    state->lastComment = null;
  }

  return res;
}

func newLocExpr(state: ParseState*, loc: SourceLoc*, kind: ExprKind) -> ExprAST* {
  let res = newExpr(state->astAlloc, kind);
  res->location = loc;
  return res;
}

func newCurLocExpr(state: ParseState*, kind: ExprKind) -> ExprAST* {
  let res = newExpr(state->astAlloc, kind);
  res->location = state->curToken.location;
  return res;
}

func newLocStmt(state: ParseState*, kind: StmtKind) -> StmtAST* {
  let res = newStmt(state->astAlloc, kind);
  res->location = state->curToken.location;

  // opt: Only parse comments if concrete is enabled.
  if (state->concrete) {
    res->comments = state->comments;
    state->comments = null;
    state->lastComment = null;
  }

  return res;
}
