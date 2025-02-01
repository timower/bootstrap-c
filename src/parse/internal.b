import ast;
import ast.print;
import util;

import state;
import token;
import decl;

func parseTopLevel(state: ParseState*) -> DeclAST* {
  getNextToken(state);  // Prep token parser

  let lastDecl: DeclAST* = null;
  let firstDecl: DeclAST* = null;

  while (state->curToken.kind != TokenKind::TOK_EOF) {
    let decl = parseDecl(state);

    if (lastDecl == null) {
      firstDecl = decl;
    } else {
      lastDecl->next = decl;
    }

    lastDecl = decl;
  }

  if (lastDecl != null) {
    lastDecl->comments = appendComments(lastDecl->comments, state->comments);
  }

  return firstDecl;
}

