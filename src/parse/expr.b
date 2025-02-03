import state;
import token;
import type;


// number := [0-9]+ | '[\n\t\r\\'"]' | '.'
func parseNumber(state: ParseState*) -> ExprAST* {
  let result = newLocExpr(state, ExprKind::INT);
  result->op = state->curToken;

  result->value = parseInteger(state->curToken);
  result->type = getInt32();

  getNextToken(state);
  return result;
}


// string := '"' [^"]* '"'
func parseString(state: ParseState*) -> ExprAST* {
  let result = newLocExpr(state, ExprKind::STR);
  result->identifier = state->curToken;
  getNextToken(state);
  return result;
}


// structInit = '{' ( ident '=' cond ','  )* ','? '}'
func parseStructInit(state: ParseState*) -> ExprAST* {
  let expr = newLocExpr(state, ExprKind::STRUCT);
  getNextToken(state);  // eat '{'

  let cur = expr;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    cur->rhs = newLocExpr(state, ExprKind::STRUCT);    // dummy struct expr
    cur = cur->rhs;

    expect(state, TokenKind::IDENTIFIER);
    cur->identifier = getNextToken(state);    // field name

    expect(state, TokenKind::EQ);
    getNextToken(state);

    cur->lhs = parseConditional(state);

    // close without trailing comma
    if (match(state, TokenKind::CLOSE_BRACE)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state);    // eat ,
  }
  getNextToken(state);  // eat }
  cur->rhs = null;

  return expr;
}


// identExpr := identifier
//           | identifier '::' identifier
//           | identifier '{' assign* '}'
func parseIdentifierExpr(state: ParseState*) -> ExprAST* {
  let loc = getLocation(state);
  let ident = getNextToken(state);

  switch (state->curToken.kind) {
    case TokenKind::OPEN_BRACE:
      let res = parseStructInit(state);
      res->identifier = ident;
      return res;

    case TokenKind::SCOPE:
      getNextToken(state);      // eat ::

      expect(state, TokenKind::IDENTIFIER);
      let loc = getLocation(state);
      let member = getNextToken(state);

      if (!match(state, TokenKind::OPEN_BRACE)) {
        let result = newLocExpr(state, ExprKind::SCOPE);
        result->location = loc;
        result->parent = ident;
        result->identifier = member;
        return result;
      }

      let res = parseStructInit(state);
      res->parent = ident;
      res->identifier = member;
      return res;

    default:
      let result = newLocExpr(state, ExprKind::VARIABLE);
      result->location = loc;
      result->identifier = ident;
      return result;
  }
}


// paren := '(' expression ')'
func parseParen(state: ParseState*) -> ExprAST* {
  getNextToken(state);  // eat (

  let expr = parseExpression(state);

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);  // eat )
  if (!state->options.concrete) {
    return expr;
  }

  let res = newLocExpr(state, ExprKind::PAREN);
  res->location = expr->location;  // TODO: use location of (
  res->lhs = expr;
  return res;
}


// primary := identExpr
//          | number
//          | string
//          | paren
func parsePrimary(state: ParseState*) -> ExprAST* {
  switch (state->curToken.kind) {
    case TokenKind::TRUE, TokenKind::FALSE:
      let expr = newLocExpr(state, ExprKind::INT);
      expr->op = getNextToken(state);
      expr->value = expr->op.kind == TokenKind::TRUE ? 1 : 0;
      expr->type = getBool();
      return expr;
    case TokenKind::IDENTIFIER:
      return parseIdentifierExpr(state);
    case TokenKind::CONSTANT:
      return parseNumber(state);
    case TokenKind::STRING_LITERAL:
      return parseString(state);
    case TokenKind::OPEN_PAREN:
      return parseParen(state);
    default:
      failParse(state, "Unknow primary expression");
      return null;
  }
}


// index := lhs '[' expression ']'
func parseIndex(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  let expr = newLocExpr(state, ExprKind::INDEX);
  getNextToken(state);  // eat [

  expr->lhs = lhs;

  expr->rhs = parseExpression(state);

  expect(state, TokenKind::CLOSE_BRACKET);
  getNextToken(state);

  return expr;
}


// call := lhs '(' [assignment (',' assigment)*] ')'
func parseCall(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  let expr = newLocExpr(state, ExprKind::CALL);
  getNextToken(state);  // eat (

  expr->lhs = lhs;
  expr->rhs = null;

  if (match(state, TokenKind::CLOSE_PAREN)) {
    getNextToken(state);
    return expr;
  }

  let cur = expr;
  while (true) {
    cur->rhs = newLocExpr(state, ExprKind::ARG_LIST);
    cur = cur->rhs;

    cur->lhs = parseAssignment(state);
    cur->rhs = null;

    if (!match(state, TokenKind::COMMA)) {
      break;
    }
    getNextToken(state);
  }

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  return expr;
}


// member := lhs ['.' | '->'] identifier
func parseMember(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  let expr = newLocExpr(state, ExprKind::MEMBER);
  expr->lhs = lhs;

  expr->op = state->curToken;
  getNextToken(state);

  expect(state, TokenKind::IDENTIFIER);
  expr->identifier = state->curToken;
  getNextToken(state);
  return expr;
}


// unary_postfix := lhs '++' | lhs '--'
func parseUnaryPostfix(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  let expr = newLocExpr(state, ExprKind::UNARY);
  expr->lhs = lhs;
  expr->rhs = null;

  expr->op = state->curToken;
  getNextToken(state);

  return expr;
}


// postfix := primary ( [index | call | member | unary_postfix] )*
func parsePostfix(state: ParseState*) -> ExprAST* {
  let expr = parsePrimary(state);

  while (true) {
    if (expr == null) {
      return expr;
    }

    switch (state->curToken.kind) {
      case TokenKind::OPEN_BRACKET:
        expr = parseIndex(state, expr);
      case TokenKind::OPEN_PAREN:
        expr = parseCall(state, expr);
      case TokenKind::DOT, TokenKind::PTR_OP:
        expr = parseMember(state, expr);
      case TokenKind::INC_OP, TokenKind::DEC_OP:
        expr = parseUnaryPostfix(state, expr);
      default:
        return expr;
    }
  }
  return expr;
}


func isUnary(tok: Token) -> bool {
  switch (tok.kind) {
    case TokenKind::INC_OP, TokenKind::DEC_OP, TokenKind::AND, TokenKind::STAR,
         TokenKind::PLUS, TokenKind::MINUS, TokenKind::TILDE, TokenKind::BANG:
      return true;
    default:
      return false;
  }
}


// unary := postfix
//        | '++' unary
//        | '--' unary
//        | '&' unary
//        | '*' unary
//        | '+' unary
//        | '-' unary
//        | '~' unary
//        | '!' unary
//        | sizeof '(' unary ')'
//        | sizeof '(' decl ')'
func parseUnary(state: ParseState*) -> ExprAST* {
  if (isUnary(state->curToken)) {
    let expr = newLocExpr(state, ExprKind::UNARY);
    expr->op = state->curToken;
    getNextToken(state);
    expr->lhs = null;
    expr->rhs = parseUnary(state);
    return expr;
  }

  if (match(state, TokenKind::SIZEOF)) {
    let expr = newLocExpr(state, ExprKind::SIZEOF);
    getNextToken(state);

    expect(state, TokenKind::OPEN_PAREN);
    getNextToken(state);

    // TODO: fix...
    if (isDecl(state->curToken) && !match(state, TokenKind::LET)) {
      expr->sizeofArg = parseType(state);
    } else {
      expr->lhs = null;
      expr->rhs = parseUnary(state);
    }

    expect(state, TokenKind::CLOSE_PAREN);
    getNextToken(state);

    return expr;
  }

  return parsePostfix(state);
}


// cast := unary | unary 'as' type
func parseCast(state: ParseState*) -> ExprAST* {
  let lhs = parseUnary(state);

  if (!match(state, TokenKind::AS)) {
    return lhs;
  }
  getNextToken(state);

  let expr = newLocExpr(state, ExprKind::CAST);
  expr->lhs = lhs;
  expr->type = parseType(state);
  return expr;
}


// binary_rhs := lhs ( op cast )*
func parseBinOpRhs(
    state: ParseState*,
    prec: i32,
    lhs: ExprAST*
) -> ExprAST* {
  while (true) {
    let curPred = getBinOpPrecedence(state->curToken);
    if (curPred < prec) {
      return lhs;
    }

    let op = state->curToken;
    let loc = getLocation(state);
    getNextToken(state);

    let rhs = parseCast(state);

    let nextPred = getBinOpPrecedence(state->curToken);
    if (curPred < nextPred) {
      rhs = parseBinOpRhs(state, curPred + 1, rhs);
    }

    let newLhs = newLocExpr(state, ExprKind::BINARY);
    newLhs->location = loc;
    newLhs->op = op;
    newLhs->lhs = lhs;
    newLhs->rhs = rhs;

    lhs = newLhs;
  }
}


// binary := cast (op cast)*
func parseBinOp(state: ParseState*) -> ExprAST* {
  let lhs = parseCast(state);
  return parseBinOpRhs(state, 0, lhs);
}


// conditional := binary
//              | binary '?' expression ':' conditional
func parseConditional(state: ParseState*) -> ExprAST* {
  let cond = parseBinOp(state);
  if (!match(state, TokenKind::QUESTION)) {
    return cond;
  }
  getNextToken(state);

  let trueBranch = parseExpression(state);
  expect(state, TokenKind::COLON);
  getNextToken(state);
  let falseBranch = parseConditional(state);

  let expr = newLocExpr(state, ExprKind::CONDITIONAL);
  expr->location = cond->location;
  expr->cond = cond;
  expr->lhs = trueBranch;
  expr->rhs = falseBranch;
  return expr;
}


// expression := assignment (',' assignment)*
func parseExpression(state: ParseState*) -> ExprAST* {
  let expr = parseAssignment(state);
  while (match(state, TokenKind::COMMA)) {
    let new = newLocExpr(state, ExprKind::BINARY);
    let op = getNextToken(state);
    let rhs = parseAssignment(state);

    new->lhs = expr;
    new->op = op;
    new->rhs = rhs;

    expr = new;
  }
  return expr;
}


// struct DeclAST *parseNoInitDecl(state: ParseState*);
// initializer := assignment | '{' assignment (',' assignment)* ','? '}'
func parseInitializer(state: ParseState*) -> ExprAST* {
  if (!match(state, TokenKind::OPEN_BRACE)) {
    return parseAssignment(state);
  }

  let expr = newLocExpr(state, ExprKind::ARRAY);
  getNextToken(state);  // eat '{'

  let cur = expr;
  while (true) {
    // Should be parseInitializer(state), but let's not support nested inits.
    cur->lhs = parseAssignment(state);

    // close without trailing comma
    if (match(state, TokenKind::CLOSE_BRACE)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    let loc = getLocation(state);
    getNextToken(state);    // eat ,

    // close with trailing comma
    if (match(state, TokenKind::CLOSE_BRACE)) {
      break;
    }

    cur->rhs = newLocExpr(state, ExprKind::ARRAY);
    cur = cur->rhs;

    // Use the ',' as location.
    cur->location = loc;
  }
  getNextToken(state);  // eat }
  cur->rhs = null;

  return expr;
}


// let_decl := 'let' identifier [':' type] ['=' initializer] ';'
func parseLetDecl(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::VAR);
  getNextToken(state);  // eat let

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  if (match(state, TokenKind::COLON)) {
    getNextToken(state);
    decl->type = parseType(state);
  } else {
    // Without type we need an init.
    expect(state, TokenKind::EQ);
  }

  if (match(state, TokenKind::EQ)) {
    getNextToken(state);
    decl->init = parseInitializer(state);
  }

  decl->endLocation = getLocation(state);

  return decl;
}


// let_expr := 'let' identifier (':' type)? '=' assignment
func parseLetExpr(state: ParseState*) -> ExprAST* {
  let expr = newLocExpr(state, ExprKind::LET);

  expr->decl = parseLetDecl(state);

  return expr;
}


// assignment := let_expr | conditional | conditional '=' assignment
func parseAssignment(state: ParseState*) -> ExprAST* {
  if (match(state, TokenKind::LET)) {
    return parseLetExpr(state);
  }

  let lhs = parseConditional(state);
  if (!isAssign(state->curToken)) {
    return lhs;
  }

  let op = getNextToken(state);
  let rhs = parseAssignment(state);
  let expr = newLocExpr(state, ExprKind::BINARY);
  expr->location = lhs->location;
  expr->op = op;
  expr->lhs = lhs;
  expr->rhs = rhs;
  return expr;
}
