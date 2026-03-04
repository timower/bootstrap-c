import state;
import token;

import expr;


// Add any comments in state that are on the same line as decl to decl.
func addTrailingCommentsStmt(state: ParseState*, stmt: StmtAST*) {
  let comments = getLineComments(state, stmt->endLocation->line);
  if (comments == null) {
    return;
    // opt: appendComments will work with a null arg, but this is faster.
  }

  stmt->comments = appendComments(stmt->comments, comments);
}

func parseCompoundStmt(state: ParseState*) -> StmtAST* {
  let stmt = newLocStmt(state, StmtKind::Compound {});
  getNextToken(state);  // eat {

  let firstStmt: StmtAST* = null;
  let cur: StmtAST* = null;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    let nextStmt = parseStmt(state);
    if (firstStmt == null) {
      firstStmt = nextStmt;
      cur = nextStmt;
    } else {
      cur->next = nextStmt;
      cur = nextStmt;
    }
  }

  // eat }
  stmt->endLocation = getNextToken(state).location;
  addTrailingCommentsStmt(state, stmt);

  (&stmt->kind as StmtKind::Compound*)->stmt = firstStmt;
  return stmt;
}

func parseExprStmt(state: ParseState*) -> StmtAST* {
  let stmt = newLocStmt(state, StmtKind::Expr {});

  if (!match(state, TokenKind::SEMICOLON)) {
    (&stmt->kind as StmtKind::Expr*)->expr = parseExpression(state);
  }

  expect(state, TokenKind::SEMICOLON);
  stmt->endLocation = getNextToken(state).location;

  addTrailingCommentsStmt(state, stmt);

  return stmt;
}


func parseForStmt(state: ParseState*) -> StmtAST* {
  getNextToken(state);  // eat for
  let stmt = newLocStmt(state, StmtKind::For {});

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let forStmt = &stmt->kind as StmtKind::For*;
  forStmt->init = parseExprStmt(state);
  forStmt->cond = parseExprStmt(state);
  forStmt->update = parseExpression(state);

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  forStmt->body = parseStmt(state);
  stmt->endLocation = forStmt->body->endLocation;

  return stmt;
}

func parseIfStmt(state: ParseState*) -> StmtAST* {
  getNextToken(state);  // eat if

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let stmt = newLocStmt(state, StmtKind::If {});

  let ifStmt = &stmt->kind as StmtKind::If*;
  ifStmt->cond = parseExpression(state);
  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  ifStmt->thenStmt = parseStmt(state);
  stmt->endLocation = ifStmt->thenStmt->endLocation;

  if (match(state, TokenKind::ELSE)) {
    getNextToken(state);
    ifStmt->elseStmt = parseStmt(state);
    stmt->endLocation = ifStmt->elseStmt->endLocation;
  }

  return stmt;
}

func parseReturnStmt(state: ParseState*) -> StmtAST* {
  getNextToken(state);

  let stmt = newLocStmt(state, StmtKind::Return {});

  // parse value
  if (!match(state, TokenKind::SEMICOLON)) {
    (&stmt->kind as StmtKind::Return*)->expr = parseExpression(state);
  }

  expect(state, TokenKind::SEMICOLON);
  stmt->endLocation = getNextToken(state).location;
  addTrailingCommentsStmt(state, stmt);
  return stmt;
}


// case_expr := primary_expr (',' primary_expr)*
//            | primary_expr 'as' identifier
func parseCaseExpr(state: ParseState*) -> ExprAST* {
  let expr = parsePrimary(state);

  if (match(state, TokenKind::AS)) {
    // TODO: this isn't really a member expr.
    let op = getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);
    let identifier = getNextToken(state);
    let res = newCurLocExpr(state, ExprKind::Member {
      object = expr,
      identifier = identifier,
      op = op,
      fieldIndex = -1,
    });

    return res;
  }

  while (match(state, TokenKind::COMMA)) {
    let op = getNextToken(state);
    let rhs = parsePrimary(state);

    expr = newCurLocExpr(state, ExprKind::Binary {
      op = op,
      lhs = expr,
      rhs = rhs,
    });
  }

  return expr;
}


// case := 'case' case_expr ':' stmt*
//       | 'default' ':' stmt*
func parseCaseOrDefault(state: ParseState*) -> StmtAST* {
  let stmt: StmtAST* = null;

  if (match(state, TokenKind::CASE)) {
    getNextToken(state);    // eat 'case'

    stmt = newLocStmt(state, StmtKind::Case {});
    (&stmt->kind as StmtKind::Case*)->expr = parseCaseExpr(state);
  } else if (match(state, TokenKind::DEFAULT)) {
    getNextToken(state);    // eat 'default'

    stmt = newLocStmt(state, StmtKind::Default {});
  } else {
    failParse(state, "Expected case or default");
  }

  expect(state, TokenKind::COLON);
  getNextToken(state);

  let firstStmt: StmtAST* = null;
  let cur: StmtAST* = null;

  // keep parsing statements until the next case or default or }
  while (!match(state, TokenKind::CASE)
      && !match(state, TokenKind::CLOSE_BRACE)
      && !match(state, TokenKind::DEFAULT)) {
    let nextStmt = parseStmt(state);
    if (firstStmt == null) {
      firstStmt = nextStmt;
      cur = nextStmt;
    } else {
      cur->next = nextStmt;
      cur = nextStmt;
    }
  }

  if (cur == null) {
    failParse(state, "Empty case not allowed");
  }

  // Set the parsed statements as the body of the case/default
  switch (stmt->kind) {
    case StmtKind::Case as caseKind:
      caseKind.body = firstStmt;
    case StmtKind::Default as defaultKind:
      defaultKind.body = firstStmt;
    default:
      unreachable("Only case or default statements expected");
  }

  stmt->endLocation = cur->endLocation;

  return stmt;
}


// switchStmt := 'switch' '(' expr ')' '{' caseStmt* '}'
func parseSwitchStmt(state: ParseState*) -> StmtAST* {
  getNextToken(state);

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let stmt = newLocStmt(state, StmtKind::Switch {});

  let switchStmt = &stmt->kind as StmtKind::Switch*;
  switchStmt->expr = parseExpression(state);
  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  // Parse list of case statements.
  expect(state, TokenKind::OPEN_BRACE);
  getNextToken(state);

  let firstCase: StmtAST* = null;
  let cur: StmtAST* = null;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    let cse = parseCaseOrDefault(state);
    if (firstCase == null) {
      firstCase = cse;
      cur = cse;
    } else {
      cur->next = cse;
      cur = cse;
    }
  }
  stmt->endLocation = getNextToken(state).location;  // eat }

  switchStmt->body = firstCase;

  addTrailingCommentsStmt(state, stmt);

  return stmt;
}

func parseWhileStmt(state: ParseState*) -> StmtAST* {
  getNextToken(state);

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let stmt = newLocStmt(state, StmtKind::While {});

  let whileStmt = &stmt->kind as StmtKind::While*;
  whileStmt->cond = parseExpression(state);
  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  whileStmt->body = parseStmt(state);
  stmt->endLocation = whileStmt->body->endLocation;

  return stmt;
}

func parseStmt(state: ParseState*) -> StmtAST* {
  if (match(state, TokenKind::OPEN_BRACE)) {
    return parseCompoundStmt(state);
  }

  if (match(state, TokenKind::FOR)) {
    return parseForStmt(state);
  }

  if (match(state, TokenKind::IF)) {
    return parseIfStmt(state);
  }

  if (match(state, TokenKind::RETURN)) {
    return parseReturnStmt(state);
  }

  if (match(state, TokenKind::SWITCH)) {
    return parseSwitchStmt(state);
  }

  if (match(state, TokenKind::WHILE)) {
    return parseWhileStmt(state);
  }

  if (match(state, TokenKind::BREAK)) {
    let stmt = newLocStmt(state, StmtKind::Break {});
    getNextToken(state);

    expect(state, TokenKind::SEMICOLON);
    stmt->endLocation = getNextToken(state).location;

    addTrailingCommentsStmt(state, stmt);

    return stmt;
  }

  if (match(state, TokenKind::CONTINUE)) {
    let stmt = newLocStmt(state, StmtKind::Continue {});
    getNextToken(state);

    expect(state, TokenKind::SEMICOLON);
    stmt->endLocation = getNextToken(state).location;

    addTrailingCommentsStmt(state, stmt);

    return stmt;
  }

  if (match(state, TokenKind::DEFER)) {
    let loc = getNextToken(state).location;
    let stmt = newStmt(StmtKind::Defer {
      stmt = parseStmt(state),
    });
    stmt->location = loc;
    stmt->endLocation = state->curToken.location;

    return stmt;
  }

  return parseExprStmt(state);
}
