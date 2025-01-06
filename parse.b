import ast;
import util;

struct ParseState {
  // [start, end[ contains the current data buffer.
  start : i8 *;
  end : i8 *;

  // Pointer in [start, end[ where we're currently parsing.
  current : i8 *;

  // Currently parsed token.
  curToken : Token;

  // current file name.
  fileName : i8 *;
};

// 1. parse
let intTypes : const i8 *[] = {
                   "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64",
};

func iseol(c : i32) -> i32 { return c == '\n' || c == '\r'; }

func failParseArg(state : ParseState *, msg : const i8 *, arg : const i8 *) {
  let line = 1;
  let column = 0;
  for (let c = state->start; c < state->current && c < state->end; c++) {
    if (iseol(*c as i32)) {
      line++;
      column = 0;
    } else {
      column++;
    }
  }

  printf("%s:%d:%d: ", state->fileName, line, column);
  if (state->curToken.data < state->end) {
    printToken(state->curToken);
  }
  printf(": %s%s\n", msg, arg);
  exit(1);
}

func failParse(state : ParseState *, msg : const i8 *) {
  failParseArg(state, msg, "");
}

// Returns the current character and advances the current pointer.
func nextChar(state : ParseState *) -> i32 {
  if (state->current >= state->end) {
    return -1;
  }

  let result = *state->current as i32;
  state->current++;
  return result;
}

// Returns the current character without advancing
func peekChar(state : ParseState *) -> i32 {
  if (state->current >= state->end) {
    return -1;
  }
  return *state->current as i32;
}

/// True if the current character is an EOL character
func is_space(c : i32) -> i32 { return iseol(c) || c == ' ' || c == '\t'; }
func is_alpha(c : i32) -> i32 {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
func is_digit(c : i32) -> i32 { return c >= '0' && c <= '9'; }
func is_alnum(c : i32) -> i32 { return is_digit(c) || is_alpha(c); }

func getToken(state : ParseState *) -> Token {
  let tokenStart = state->current;
  let lastChar = nextChar(state);

  let token : Token;

  // TODO: const expressions and make these global
  // TODO: typeof(array[0])
  // TODO: array size
  let tokenSize = (sizeof(tokens) / sizeof(tokens[0])) as i32;
  let intTypeSize = (sizeof(intTypes) / sizeof(intTypes[0])) as i32;

  // Eat whitespace
  while (is_space(lastChar)) {
    tokenStart = state->current;
    lastChar = nextChar(state);
  }

  if (lastChar == -1) {
    token.kind = TokenKind::TOK_EOF;
    return token;
  }

  // identifier [a-zA-Z][a-zA-Z0-9]*
  if (is_alpha(lastChar) || lastChar == '_') {
    while (is_alnum(peekChar(state)) || peekChar(state) == '_') {
      nextChar(state);
    }

    token.data = tokenStart;
    token.end = state->current; // one past the end!

    // Check if it's a keyword.
    for (let i = TokenKind::CONTINUE as i32; i < tokenSize; i++) {
      if (tokCmpStr(token, tokens[i])) {
        token.kind = i as enum TokenKind;
        return token;
      }
    }

    // i32 types [iu](8|16|32|64)
    for (let i = 0; i < intTypeSize; i++) {
      if (tokCmpStr(token, intTypes[i])) {
        token.kind = TokenKind::INT2;
        return token;
      }
    }

    token.kind = TokenKind::IDENTIFIER;
    return token;
  }

  if (lastChar == '\'') {
    while (peekChar(state) != '\'') {
      let next = nextChar(state);
      if (next == '\\') {
        nextChar(state);
      }
    }
    token.data = tokenStart;
    nextChar(state); // eat closing '
    token.end = state->current;
    token.kind = TokenKind::CONSTANT;
    return token;
  }

  if (lastChar == '"') {
    while (peekChar(state) != '"') {
      let next = nextChar(state);
      if (next == '\\') {
        nextChar(state);
      }
    }
    token.data = tokenStart + 1; // eat the starting "
    token.end = state->current;

    nextChar(state); // eat closing "
    token.kind = TokenKind::STRING_LITERAL;
    return token;
  }

  if (is_digit(lastChar) || (lastChar == '-' && is_digit(peekChar(state)))) {
    while (is_digit(peekChar(state)) || peekChar(state) == '.') {
      nextChar(state);
    }
    token.data = tokenStart;
    token.end = state->current;
    token.kind = TokenKind::CONSTANT;
    return token;
  }

  // pre-processor
  if (lastChar == '#') {
    while (!iseol(peekChar(state))) {
      nextChar(state);
    }
    return getToken(state);
  }

  // Comments //
  if (lastChar == '/' && peekChar(state) == '/') {
    while (!iseol(peekChar(state))) {
      nextChar(state);
    }
    return getToken(state);
  }

  // Asume operator
  for (let i = TokenKind::CONTINUE as i32; i < tokenSize; i++) {
    let len = strlen(tokens[i]) as i64;
    let remaining = state->end - tokenStart;
    if (len < remaining && memcmp(tokenStart, tokens[i], len as u64) == 0) {
      token.kind = i as enum TokenKind;
      token.data = tokenStart;

      state->current = tokenStart + len;
      token.end = state->current;

      return token;
    }
  }

  printf("Unknown token! %x\n", lastChar);
  failParse(state, "Unknown token");
  token.kind = TokenKind::TOK_EOF;
  return token;
}

func match(state : ParseState *, tok : TokenKind) -> i32 {
  return state->curToken.kind == tok;
}

func expect(state : ParseState *, tok : TokenKind) {
  if (!match(state, tok)) {
    failParseArg(state, "Expected: ", tokens[tok as i32]);
  }
}

func getNextToken(state : ParseState *) -> Token {
  let result = state->curToken;
  state->curToken = getToken(state);
  return result;
}

// number := [0-9]+ | '[\n\t\r\\'"]' | '.'
func parseNumber(state : ParseState *) -> ExprAST * {
  let result = newExpr(ExprKind::INT);

  let start = state->curToken.data;
  if (*start == '\'') {
    let next = *(start + 1);
    if (next == '\\') {
      result->value = getEscaped(*(start + 2)) as i32;
    } else {
      result->value = next as i32;
    }
  } else {
    let endp = state->curToken.end;
    let num = strtol(start, &endp, 10) as i32;
    result->value = num;
  }

  getNextToken(state);
  return result;
}

// string := '"' [^"]* '"'
func parseString(state : ParseState *) -> ExprAST * {
  let result = newExpr(ExprKind::STR);
  result->identifier = state->curToken;
  getNextToken(state);
  return result;
}

func parseAssignment(state : ParseState *) -> ExprAST *;
func parseConditional(state : ParseState *) -> ExprAST *;

// structInit = '{' ( ident '=' cond ','  )* ','? '}'
func parseStructInit(state : ParseState *, ident : Token) -> ExprAST * {
  getNextToken(state); // eat '{'

  let expr = newExpr(ExprKind::STRUCT);
  expr->identifier = ident;

  let cur = expr;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    cur->rhs = newExpr(ExprKind::STRUCT); // dummy struct expr
    cur = cur->rhs;

    expect(state, TokenKind::IDENTIFIER);
    cur->identifier = getNextToken(state); // field name

    expect(state, TokenKind::EQ);
    getNextToken(state);

    cur->lhs = parseConditional(state);

    // close without trailing comma
    if (match(state, TokenKind::CLOSE_BRACE)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state); // eat ,
  }
  getNextToken(state); // eat }
  cur->rhs = NULL;

  return expr;
}

// identExpr := identifier
//           | identifier '::' identifier
//           | identifier '{' assign* '}'
func parseIdentifierExpr(state : ParseState *) -> ExprAST * {
  let ident = getNextToken(state);

  switch (state->curToken.kind) {
  case TokenKind::OPEN_BRACE:
    return parseStructInit(state, ident);

  case TokenKind::SCOPE:
    getNextToken(state); // eat ::
    expect(state, TokenKind::IDENTIFIER);

    let result = newExpr(ExprKind::SCOPE);
    result->parent = ident;
    result->identifier = getNextToken(state);
    return result;

  default:
    let result = newExpr(ExprKind::VARIABLE);
    result->identifier = ident;
    return result;
  }
}

func parseExpression(state : ParseState *) -> ExprAST *;

// paren := '(' expression ')'
func parseParen(state : ParseState *) -> ExprAST * {
  getNextToken(state); // eat (

  let expr = parseExpression(state);

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state); // eat )
  return expr;
}

// primary := identExpr
//          | number
//          | string
//          | paren
func parsePrimary(state : ParseState *) -> ExprAST * {
  switch (state->curToken.kind) {
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
    return NULL;
  }
}

// index := lhs '[' expression ']'
func parseIndex(state : ParseState *, lhs : ExprAST *) -> ExprAST * {
  getNextToken(state); // eat [

  let expr = newExpr(ExprKind::INDEX);
  expr->lhs = lhs;

  expr->rhs = parseExpression(state);

  expect(state, TokenKind::CLOSE_BRACKET);
  getNextToken(state);

  return expr;
}

// call := lhs '(' [assignment (',' assigment)*] ')'
func parseCall(state : ParseState *, lhs : ExprAST *) -> ExprAST * {
  getNextToken(state); // eat (

  let expr = newExpr(ExprKind::CALL);

  expr->lhs = lhs;
  expr->rhs = NULL;

  if (match(state, TokenKind::CLOSE_PAREN)) {
    getNextToken(state);
    return expr;
  }

  let cur = expr;
  while (1) {
    cur->rhs = newExpr(ExprKind::ARG_LIST);
    cur = cur->rhs;

    cur->lhs = parseAssignment(state);
    cur->rhs = NULL;

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
func parseMember(state : ParseState *, lhs : ExprAST *) -> ExprAST * {
  let expr = newExpr(ExprKind::MEMBER);
  expr->lhs = lhs;

  expr->op = state->curToken;
  getNextToken(state);

  expect(state, TokenKind::IDENTIFIER);
  expr->identifier = state->curToken;
  getNextToken(state);
  return expr;
}

// unary_postfix := lhs '++' | lhs '--'
func parseUnaryPostfix(state : ParseState *, lhs : ExprAST *) -> ExprAST * {
  let expr = newExpr(ExprKind::UNARY);
  expr->lhs = lhs;
  expr->rhs = NULL;

  expr->op = state->curToken;
  getNextToken(state);

  return expr;
}

// postfix := primary ( [index | call | member | unary_postfix] )*
func parsePostfix(state : ParseState *) -> ExprAST * {
  let expr = parsePrimary(state);

  while (1) {
    if (expr == NULL) {
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

func isDecl(tok : Token) -> i32 {
  // We don't support typedef, so this is easy
  switch (tok.kind) {
  case TokenKind::STRUCT, TokenKind::ENUM, TokenKind::LET, TokenKind::FUNC:
    return 1;
  default:
    return 0;
  }
}

func isUnary(tok : Token) -> i32 {
  switch (tok.kind) {
  case TokenKind::INC_OP, TokenKind::DEC_OP, TokenKind::AND, TokenKind::STAR,
      TokenKind::PLUS, TokenKind::MINUS, TokenKind::TILDE, TokenKind::BANG:
    return 1;
  default:
    return 0;
  }
}

// base_type := int2 | 'void' | 'struct' ident | 'enum' ident | ident
// type := const? base_type ('*' | '[' int? ']' )*
func parseType(state : ParseState *) -> Type * {
  let type = newType(TypeKind::INT);

  if (match(state, TokenKind::CONST)) {
    getNextToken(state);
    type->isConst = 1;
  }

  if (match(state, TokenKind::INT2)) {
    type->kind = TypeKind::INT;
    type->isSigned = *state->curToken.data == 'i';
    let end = state->curToken.end;
    type->size = strtol(state->curToken.data + 1, &end, 10) as i32;
    getNextToken(state);
  } else if (match(state, TokenKind::VOID)) {
    getNextToken(state);
    type->kind = TypeKind::VOID;
  } else if (match(state, TokenKind::STRUCT)) {
    getNextToken(state);
    type->kind = TypeKind::STRUCT;
    expect(state, TokenKind::IDENTIFIER);
    type->tag = getNextToken(state);
  } else if (match(state, TokenKind::ENUM)) {
    getNextToken(state);
    type->kind = TypeKind::ENUM;
    expect(state, TokenKind::IDENTIFIER);
    type->tag = getNextToken(state);
  } else if (match(state, TokenKind::IDENTIFIER)) {
    type->kind = TypeKind::TAG;
    type->tag = getNextToken(state);
  } else {
    failParse(state, "Unknown type");
    return NULL;
  }

  // parse type suffixes (pointers & arrays)
  while (1) {
    if (match(state, TokenKind::STAR)) {
      getNextToken(state);
      let ptrType = newType(TypeKind::POINTER);
      ptrType->arg = type;
      type = ptrType;
    } else if (match(state, TokenKind::OPEN_BRACKET)) {
      getNextToken(state);

      let arrayType = newType(TypeKind::ARRAY);
      arrayType->arg = type;

      if (match(state, TokenKind::CONSTANT)) {
        let numExpr = parseNumber(state);
        arrayType->size = numExpr->value;
      } else {
        arrayType->size = -1;
      }

      expect(state, TokenKind::CLOSE_BRACKET);
      getNextToken(state);

      type = arrayType;
    } else {
      break;
    }
  }

  return type;
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
func parseUnary(state : ParseState *) -> ExprAST * {
  if (isUnary(state->curToken)) {
    let expr = newExpr(ExprKind::UNARY);
    expr->op = state->curToken;
    getNextToken(state);
    expr->lhs = NULL;
    expr->rhs = parseUnary(state);
    return expr;
  }

  if (match(state, TokenKind::SIZEOF)) {
    getNextToken(state);
    let expr = newExpr(ExprKind::SIZEOF);

    expect(state, TokenKind::OPEN_PAREN);
    getNextToken(state);

    // TODO: fix...
    if (isDecl(state->curToken) && !match(state, TokenKind::LET)) {
      expr->sizeofArg = parseType(state);
    } else {

      expr->lhs = NULL;
      expr->rhs = parseUnary(state);
    }

    expect(state, TokenKind::CLOSE_PAREN);
    getNextToken(state);

    return expr;
  }

  return parsePostfix(state);
}

// cast := unary | unary 'as' decl
func parseCast(state : ParseState *) -> ExprAST * {
  let lhs = parseUnary(state);

  if (!match(state, TokenKind::AS)) {
    return lhs;
  }
  getNextToken(state);

  let expr = newExpr(ExprKind::CAST);
  expr->lhs = lhs;
  expr->type = parseType(state);
  return expr;
}

func getPrecedence(tok : Token) -> i32 {
  switch (tok.kind) {
  case TokenKind::STAR, TokenKind::SLASH, TokenKind::PERCENT:
    return 100;

  case TokenKind::PLUS, TokenKind::MINUS:
    return 90;

  case TokenKind::LEFT_OP, TokenKind::RIGHT_OP:
    return 80;

  case TokenKind::LESS, TokenKind::GREATER, TokenKind::LE_OP, TokenKind::GE_OP:
    return 70;

  case TokenKind::EQ_OP, TokenKind::NE_OP:
    return 60;

  case TokenKind::AND:
    return 50;
  case TokenKind::HAT:
    return 40;
  case TokenKind::PIPE:
    return 30;

  case TokenKind::AND_OP:
    return 20;
  case TokenKind::OR_OP:
    return 10;

  default:
    return -1;
  }
}

// binary_rhs := lhs ( op cast )*
func parseBinOpRhs(state
                   : ParseState *, prec
                   : i32, lhs
                   : ExprAST *) -> ExprAST * {
  while (1) {
    let curPred = getPrecedence(state->curToken);
    if (curPred < prec) {
      return lhs;
    }

    let op = state->curToken;
    getNextToken(state);

    let rhs = parseCast(state);

    let nextPred = getPrecedence(state->curToken);
    if (curPred < nextPred) {
      rhs = parseBinOpRhs(state, curPred + 1, rhs);
    }

    let newLhs = newExpr(ExprKind::BINARY);
    newLhs->op = op;
    newLhs->lhs = lhs;
    newLhs->rhs = rhs;

    lhs = newLhs;
  }
}

// binary := cast (op cast)*
func parseBinOp(state : ParseState *) -> ExprAST * {
  let lhs = parseCast(state);
  return parseBinOpRhs(state, 0, lhs);
}

// conditional := binary
//              | binary '?' expression ':' conditional
func parseConditional(state : ParseState *) -> ExprAST * {
  let cond = parseBinOp(state);
  if (!match(state, TokenKind::QUESTION)) {
    return cond;
  }
  getNextToken(state);

  let trueBranch = parseExpression(state);
  expect(state, TokenKind::COLON);
  getNextToken(state);
  let falseBranch = parseConditional(state);

  let expr = newExpr(ExprKind::CONDITIONAL);
  expr->cond = cond;
  expr->lhs = trueBranch;
  expr->rhs = falseBranch;
  return expr;
}

// assignment := conditional | conditional '=' assignment
func parseAssignment(state : ParseState *) -> ExprAST * {
  let lhs = parseConditional(state);
  if (!isAssign(state->curToken)) {
    return lhs;
  }

  let op = getNextToken(state);
  let rhs = parseAssignment(state);
  let expr = newExpr(ExprKind::BINARY);
  expr->op = op;
  expr->lhs = lhs;
  expr->rhs = rhs;
  return expr;
}

// expression := assignment (',' assignment)*
func parseExpression(state : ParseState *) -> ExprAST * {
  let expr = parseAssignment(state);
  while (match(state, TokenKind::COMMA)) {
    let op = getNextToken(state);
    let rhs = parseAssignment(state);

    let new = newExpr(ExprKind::BINARY);
    new->lhs = expr;
    new->op = op;
    new->rhs = rhs;

    expr = new;
  }
  return expr;
}

func parseStmt(state : ParseState *) -> StmtAST *;

func parseCompoundStmt(state : ParseState *) -> StmtAST * {
  getNextToken(state); // eat {

  let stmt = newStmt(StmtKind::COMPOUND);

  let cur = stmt;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    cur->nextStmt = parseStmt(state);
    cur = cur->nextStmt;
  }
  getNextToken(state); // eat }

  stmt->stmt = stmt->nextStmt;
  stmt->nextStmt = NULL;
  return stmt;
}

func parseExprStmt(state : ParseState *) -> StmtAST * {
  let stmt = newStmt(StmtKind::EXPR);

  if (!match(state, TokenKind::SEMICOLON)) {
    stmt->expr = parseExpression(state);
  }

  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);

  return stmt;
}

func parseDeclarationOrFunction(state : ParseState *) -> DeclAST *;

func parseDeclStmt(state : ParseState *) -> StmtAST * {
  let stmt = newStmt(StmtKind::DECL);
  stmt->decl = parseDeclarationOrFunction(state);
  return stmt;
}

func parseForStmt(state : ParseState *) -> StmtAST * {
  getNextToken(state); // eat for
  let stmt = newStmt(StmtKind::FOR);

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  if (isDecl(state->curToken)) {
    stmt->init = parseDeclStmt(state);
  } else {
    stmt->init = parseExprStmt(state);
  }
  stmt->cond = parseExprStmt(state);
  stmt->expr = parseExpression(state);

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  stmt->stmt = parseStmt(state);

  return stmt;
}

func parseIfStmt(state : ParseState *) -> StmtAST * {
  getNextToken(state); // eat if

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let stmt = newStmt(StmtKind::IF);

  stmt->expr = parseExpression(state);
  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  stmt->init = parseStmt(state);

  if (match(state, TokenKind::ELSE)) {
    getNextToken(state);
    stmt->stmt = parseStmt(state);
  }

  return stmt;
}

func parseReturnStmt(state : ParseState *) -> StmtAST * {
  getNextToken(state);

  let stmt = newStmt(StmtKind::RETURN);
  // parse value
  if (!match(state, TokenKind::SEMICOLON)) {
    stmt->expr = parseExpression(state);
  }

  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);
  return stmt;
}

// case_expr := primary_expr (',' primary_expr)*
func parseCaseExpr(state : ParseState *) -> ExprAST * {
  let expr = parsePrimary(state);
  while (match(state, TokenKind::COMMA)) {
    let op = getNextToken(state);
    let rhs = parsePrimary(state);

    let new = newExpr(ExprKind::BINARY);
    new->lhs = expr;
    new->op = op;
    new->rhs = rhs;

    expr = new;
  }

  return expr;
}

// case := 'case' case_expr ':' stmt*
//       | 'default' ':' stmt*
func parseCaseOrDefault(state : ParseState *) -> StmtAST * {
  let stmt : StmtAST * = NULL;

  if (match(state, TokenKind::CASE)) {
    getNextToken(state); // eat 'case'

    stmt = newStmt(StmtKind::CASE);
    stmt->expr = parseCaseExpr(state);

  } else if (match(state, TokenKind::DEFAULT)) {
    getNextToken(state); // eat 'default'

    stmt = newStmt(StmtKind::DEFAULT);
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
  while (!match(state, TokenKind::CASE) &&
         !match(state, TokenKind::CLOSE_BRACE) &&
         !match(state, TokenKind::DEFAULT)) {
    let nextStmt = parseStmt(state);
    cur->nextStmt = nextStmt;
    cur = nextStmt;
  }

  stmt->stmt = stmt->nextStmt;
  stmt->nextStmt = NULL;

  return stmt;
}

// switchStmt := 'switch' '(' expr ')' '{' caseStmt* '}'
func parseSwitchStmt(state : ParseState *) -> StmtAST * {
  getNextToken(state);

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let stmt = newStmt(StmtKind::SWITCH);

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
  getNextToken(state); // eat }

  stmt->stmt = stmt->nextStmt;
  stmt->nextStmt = NULL;

  return stmt;
}

func parseWhileStmt(state : ParseState *) -> StmtAST * {
  getNextToken(state);

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let stmt = newStmt(StmtKind::WHILE);

  stmt->expr = parseExpression(state);
  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  stmt->stmt = parseStmt(state);

  return stmt;
}

func parseStmt(state : ParseState *) -> StmtAST * {
  if (isDecl(state->curToken)) {
    return parseDeclStmt(state);
  }

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
    getNextToken(state);
    let stmt = newStmt(StmtKind::BREAK);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    return stmt;
  }

  return parseExprStmt(state);
}

// struct DeclAST *parseNoInitDecl(state: ParseState*);

// initializer := assignment | '{' assignment (',' assignment)* ','? '}'
func parseInitializer(state : ParseState *) -> ExprAST * {
  if (!match(state, TokenKind::OPEN_BRACE)) {
    return parseAssignment(state);
  }
  getNextToken(state);

  let expr = newExpr(ExprKind::ARRAY);
  let cur = expr;
  while (1) {
    // Should be parseInitializer(state), but let's not support nested inits.
    cur->lhs = parseAssignment(state);

    // close without trailing comma
    if (match(state, TokenKind::CLOSE_BRACE)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state); // eat ,

    // close with trailing comma
    if (match(state, TokenKind::CLOSE_BRACE)) {
      break;
    }

    cur->rhs = newExpr(ExprKind::ARRAY);
    cur = cur->rhs;
  }
  getNextToken(state); // eat }
  cur->rhs = NULL;

  return expr;
}

// type_name_pair := identifier ':' type
func parseNameTypePair(state : ParseState *) -> DeclAST * {
  let decl = newDecl();
  decl->kind = DeclKind::VAR;

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  expect(state, TokenKind::COLON);
  getNextToken(state);

  decl->type = parseType(state);

  return decl;
}

// let_decl := 'let' identifier [':' type] ['=' initializer] ';'
func parseLetDecl(state : ParseState *) -> DeclAST * {
  getNextToken(state); // eat let

  let decl = newDecl();
  decl->kind = DeclKind::VAR;

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

  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);

  return decl;
}

// struct := 'struct' identifier '{' decl* '}'
//         | 'struct' identifier
func parseStruct(state : ParseState *, decl : DeclAST *) {
  getNextToken(state); // eat struct
  decl->type = newType(TypeKind::STRUCT);

  // (non)optional tag
  expect(state, TokenKind::IDENTIFIER);

  decl->type->tag = getNextToken(state);

  // Just a 'struct Foo' ref.
  if (!match(state, TokenKind::OPEN_BRACE)) {
    return;
  }
  getNextToken(state); // eat {

  decl->kind = DeclKind::STRUCT;

  // parse the fields
  let fields = decl;
  while (!match(state, TokenKind::CLOSE_BRACE)) {

    fields->next = parseNameTypePair(state);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state); // eat ;
    fields = fields->next;
  }
  getNextToken(state); // eat }

  fields->next = NULL;
  decl->fields = decl->next;
  decl->next = NULL;
}

// enum := 'enum' identifier '{' identifier  (',' identifier )* ','? '}'
//       | 'enum' identifier
func parseEnum(state : ParseState *, decl : DeclAST *) {
  getNextToken(state);

  decl->type = newType(TypeKind::ENUM);

  expect(state, TokenKind::IDENTIFIER);
  decl->type->tag = getNextToken(state);

  if (!match(state, TokenKind::OPEN_BRACE)) {
    return;
  }
  getNextToken(state);

  decl->kind = DeclKind::ENUM;

  // parse constants
  let fields = decl;
  let idx = 0;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    expect(state, TokenKind::IDENTIFIER);

    let field = newDecl();
    field->kind = DeclKind::ENUM_FIELD;
    field->type = getInt32();

    field->name = getNextToken(state);
    field->enumValue = idx++;

    fields->next = field;
    fields = field;

    if (match(state, TokenKind::CLOSE_BRACE)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state);
  }
  getNextToken(state); // eat }

  fields->next = NULL;
  decl->fields = decl->next;
  decl->next = NULL;
}

// func_decl :=
//  'func' identifier [ '->' type ] '(' [decl (',' decl)*] ')' compound_stmt?
func parseFuncDecl(state : ParseState *) -> DeclAST * {
  getNextToken(state); // eat func

  let decl = newDecl();
  decl->kind = DeclKind::FUNC;

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  let funcType = newType(TypeKind::FUNC);
  decl->type = funcType;

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state); // eat (

  let curType = funcType;
  let curDecl = decl;
  while (!match(state, TokenKind::CLOSE_PAREN)) {
    if (match(state, TokenKind::ELLIPSIS)) {
      getNextToken(state);
      funcType->isVarargs = 1;

      expect(state, TokenKind::CLOSE_PAREN);
      break;
    }

    let param = parseNameTypePair(state);
    curDecl->next = param;
    curDecl = param;
    curType->argNext = param->type;
    curType = param->type;

    if (match(state, TokenKind::CLOSE_PAREN)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state); // eat ,
  }
  getNextToken(state); // eat )

  decl->fields = decl->next;
  decl->next = NULL;
  funcType->arg = funcType->argNext;
  funcType->argNext = NULL;

  if (match(state, TokenKind::PTR_OP)) {
    getNextToken(state); // eat ->
    decl->type->result = parseType(state);
  } else {
    decl->type->result = newType(TypeKind::VOID);
  }

  if (match(state, TokenKind::OPEN_BRACE)) {
    decl->body = parseCompoundStmt(state);
  } else {
    expect(state, TokenKind::SEMICOLON);
    getNextToken(state); // eat ;
  }

  return decl;
}

func parseDeclarationOrFunction(state : ParseState *) -> DeclAST * {
  if (match(state, TokenKind::LET)) {
    return parseLetDecl(state);
  }

  if (match(state, TokenKind::FUNC)) {
    return parseFuncDecl(state);
  }

  if (match(state, TokenKind::STRUCT)) {
    let decl = newDecl();
    parseStruct(state, decl);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    return decl;
  }

  if (match(state, TokenKind::ENUM)) {
    let decl = newDecl();
    parseEnum(state, decl);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    return decl;
  }

  failParse(state, "Unknown declaration");
  return NULL;
}

func parseImportDecl(state : ParseState *) -> DeclAST * {
  getNextToken(state); // eat import

  expect(state, TokenKind::IDENTIFIER);
  let name = getNextToken(state);

  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);

  let decl = newDecl();
  decl->kind = DeclKind::IMPORT;
  decl->name = name;
  return decl;
}

func parseTopLevelDecl(state : ParseState *) -> DeclAST * {
  if (match(state, TokenKind::IMPORT)) {
    return parseImportDecl(state);
  }
  return parseDeclarationOrFunction(state);
}

func parseTopLevel(state : ParseState *) -> DeclAST * {
  getNextToken(state); // Prep token parser

  let lastDecl : DeclAST * = NULL;
  let firstDecl : DeclAST * = NULL;

  while (state->curToken.kind != TokenKind::TOK_EOF) {
    let decl = parseTopLevelDecl(state);

    if (lastDecl == NULL) {
      firstDecl = decl;
    } else {
      lastDecl->next = decl;
    }

    lastDecl = decl;
  }

  return firstDecl;
}

func parseFile(name : const i8 *) -> DeclAST * {
  let fd = open(name, 0); //  O_RDONLY
  if (fd == -1) {
    puts("open failed!");
    return NULL;
  }

  let size = lseek(fd, 0, 2); //  SEEK_END
  if (size == -1) {
    puts("seek failed!");
    return NULL;
  }

  if (lseek(fd, 0, 0) == -1) { // SEEK_SET
    puts("seek failed!");
    return NULL;
  }

  let fileMem : i8 * = malloc(size as u64);

  let off : i64 = 0;
  while (off != size) {
    let r = read(fd, fileMem + off, (size - off) as u64);
    if (r == -1) {
      puts("read failed!");
      return NULL;
    }
    off += r;
  }

  let parseState : ParseState = {0};
  parseState.fileName = name;
  parseState.current = parseState.start = fileMem;
  parseState.end = fileMem + size;

  return parseTopLevel(&parseState);
}
