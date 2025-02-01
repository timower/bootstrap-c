import state;
import token;

import expr;


// Add any comments in state that are on the same line as decl to decl.
func addTrailingCommentsStmt(state: ParseState*, stmt: StmtAST*) {
  let comments = getLineComments(state, stmt->endLocation.line);
  if (comments == null) {
    return;
  }

  stmt->comments = appendComments(stmt->comments, comments);
}

func parseCompoundStmt(state: ParseState*) -> StmtAST* {
  let stmt = newLocStmt(state, StmtKind::COMPOUND);
  getNextToken(state);  // eat {

  let cur = stmt;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    cur->nextStmt = parseStmt(state);
    cur = cur->nextStmt;
  }
  stmt->endLocation = getLocation(state);
  addTrailingCommentsStmt(state, stmt);

  getNextToken(state);  // eat }

  stmt->stmt = stmt->nextStmt;
  stmt->nextStmt = null;
  return stmt;
}

func parseExprStmt(state: ParseState*) -> StmtAST* {
  let stmt = newLocStmt(state, StmtKind::EXPR);

  if (!match(state, TokenKind::SEMICOLON)) {
    stmt->expr = parseExpression(state);
  }

  expect(state, TokenKind::SEMICOLON);
  stmt->endLocation = getLocation(state);
  getNextToken(state);

  addTrailingCommentsStmt(state, stmt);

  return stmt;
}


func parseForStmt(state: ParseState*) -> StmtAST* {
  getNextToken(state);  // eat for
  let stmt = newLocStmt(state, StmtKind::FOR);

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  stmt->init = parseExprStmt(state);
  stmt->cond = parseExprStmt(state);
  stmt->expr = parseExpression(state);

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  stmt->stmt = parseStmt(state);
  stmt->endLocation = stmt->stmt->endLocation;

  return stmt;
}

func parseIfStmt(state: ParseState*) -> StmtAST* {
  getNextToken(state);  // eat if

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let stmt = newLocStmt(state, StmtKind::IF);

  stmt->expr = parseExpression(state);
  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  stmt->init = parseStmt(state);
  stmt->endLocation = stmt->init->endLocation;

  if (match(state, TokenKind::ELSE)) {
    getNextToken(state);
    stmt->stmt = parseStmt(state);
    stmt->endLocation = stmt->stmt->endLocation;
  }

  return stmt;
}

func parseReturnStmt(state: ParseState*) -> StmtAST* {
  getNextToken(state);

  let stmt = newLocStmt(state, StmtKind::RETURN);

  // parse value
  if (!match(state, TokenKind::SEMICOLON)) {
    stmt->expr = parseExpression(state);
  }

  expect(state, TokenKind::SEMICOLON);
  stmt->endLocation = getLocation(state);
  getNextToken(state);
  addTrailingCommentsStmt(state, stmt);
  return stmt;
}


// case_expr := primary_expr (',' primary_expr)*
//            | primary_expr 'as' identifier
func parseCaseExpr(state: ParseState*) -> ExprAST* {
  let expr = parsePrimary(state);

  if (match(state, TokenKind::AS)) {
    // TODO: this isn't really a member expr.
    let res = newLocExpr(state, ExprKind::MEMBER);
    res->op = getNextToken(state);
    res->lhs = expr;

    expect(state, TokenKind::IDENTIFIER);
    res->identifier = getNextToken(state);

    return res;
  }

  while (match(state, TokenKind::COMMA)) {
    let op = getNextToken(state);
    let rhs = parsePrimary(state);

    let new = newLocExpr(state, ExprKind::BINARY);
    new->lhs = expr;
    new->op = op;
    new->rhs = rhs;

    expr = new;
  }

  return expr;
}


// case := 'case' case_expr ':' stmt*
//       | 'default' ':' stmt*
func parseCaseOrDefault(state: ParseState*) -> StmtAST* {
  let stmt: StmtAST* = null;

  if (match(state, TokenKind::CASE)) {
    getNextToken(state);    // eat 'case'

    stmt = newLocStmt(state, StmtKind::CASE);
    stmt->expr = parseCaseExpr(state);
  } else if (match(state, TokenKind::DEFAULT)) {
    getNextToken(state);    // eat 'default'

    stmt = newLocStmt(state, StmtKind::DEFAULT);
  } else {
    failParse(state, "Expected case or default");
  }

  expect(state, TokenKind::COLON);
  getNextToken(state);

  if (match(state, TokenKind::CASE) || match(state, TokenKind::DEFAULT)) {
    failParse(state, "Empty case not allowed, use break");
  }

  let cur = stmt;

  // keep parsing statements until the next case or default or }
  while (!match(state, TokenKind::CASE)
      && !match(state, TokenKind::CLOSE_BRACE)
      && !match(state, TokenKind::DEFAULT)) {
    let nextStmt = parseStmt(state);
    cur->nextStmt = nextStmt;
    cur = nextStmt;
  }

  stmt->stmt = stmt->nextStmt;
  stmt->nextStmt = null;
  stmt->endLocation = cur->endLocation;

  return stmt;
}


// switchStmt := 'switch' '(' expr ')' '{' caseStmt* '}'
func parseSwitchStmt(state: ParseState*) -> StmtAST* {
  getNextToken(state);

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let stmt = newLocStmt(state, StmtKind::SWITCH);

  stmt->expr = parseExpression(state);
  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  // Parse list of case statements.
  expect(state, TokenKind::OPEN_BRACE);
  getNextToken(state);

  let cur = stmt;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    let cse = parseCaseOrDefault(state);
    cur->nextStmt = cse;
    cur = cse;
  }
  stmt->endLocation = getLocation(state);
  getNextToken(state);  // eat }

  stmt->stmt = stmt->nextStmt;
  stmt->nextStmt = null;

  addTrailingCommentsStmt(state, stmt);

  return stmt;
}

func parseWhileStmt(state: ParseState*) -> StmtAST* {
  getNextToken(state);

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let stmt = newLocStmt(state, StmtKind::WHILE);

  stmt->expr = parseExpression(state);
  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  stmt->stmt = parseStmt(state);
  stmt->endLocation = stmt->stmt->endLocation;

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
    let stmt = newLocStmt(state, StmtKind::BREAK);
    getNextToken(state);

    expect(state, TokenKind::SEMICOLON);
    stmt->endLocation = getLocation(state);
    getNextToken(state);

    addTrailingCommentsStmt(state, stmt);

    return stmt;
  }

  return parseExprStmt(state);
}
