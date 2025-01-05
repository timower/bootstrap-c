import ast;
import util;

struct ParseState {
  // [start, end[ contains the current data buffer.
  i8 *start;
  i8 *end;

  // Pointer in [start, end[ where we're currently parsing.
  i8 *current;

  // Currently parsed token.
  struct Token curToken;
};

// 1. parse
const i8 *intTypes[] = {
    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64",
};

i32 iseol(i32 c) { return c == '\n' || c == '\r'; }

void failParseArg(struct ParseState *state, const i8 *msg, const i8 *arg) {
  i32 line = 1;
  i32 column = 0;
  for (const i8 *c = state->start; c < state->current && c < state->end; c++) {
    if (iseol(*c as i32)) {
      line++;
      column = 0;
    } else {
      column++;
    }
  }

  printf("%d:%d: ", line, column);
  if (state->curToken.data < state->end) {
    printToken(state->curToken);
  }
  printf(": %s%s\n", msg, arg);
  exit(1);
}

void failParse(struct ParseState *state, const i8 *msg) {
  failParseArg(state, msg, "");
}

// Returns the current character and advances the current pointer.
i32 nextChar(struct ParseState *state) {
  if (state->current >= state->end) {
    return -1;
  }

  i32 result = *state->current as i32;
  state->current++;
  return result;
}

// Returns the current character without advancing
i32 peekChar(struct ParseState *state) {
  if (state->current >= state->end) {
    return -1;
  }
  return *state->current as i32;
}

/// True if the current character is an EOL character
i32 is_space(i32 c) { return iseol(c) || c == ' ' || c == '\t'; }
i32 is_alpha(i32 c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }
i32 is_digit(i32 c) { return c >= '0' && c <= '9'; }
i32 is_alnum(i32 c) { return is_digit(c) || is_alpha(c); }

struct Token getToken(struct ParseState *state) {
  i8 *tokenStart = state->current;
  i32 lastChar = nextChar(state);
  struct Token token;

  // TODO: const expressions and make these global
  const i32 tokenSize = (sizeof(tokens) / sizeof(tokens[0])) as i32;
  const i32 intTypeSize = (sizeof(intTypes) / sizeof(intTypes[0])) as i32;

  // Eat whitespace
  while (is_space(lastChar)) {
    tokenStart = state->current;
    lastChar = nextChar(state);
  }

  if (lastChar == -1) {
    token.kind = TOK_EOF;
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
    for (i32 i = CONTINUE; i < tokenSize; i++) {
      if (tokCmpStr(token, tokens[i])) {
        token.kind = i;
        return token;
      }
    }

    // i32 types [iu](8|16|32|64)
    for (i32 i = 0; i < intTypeSize; i++) {
      if (tokCmpStr(token, intTypes[i])) {
        token.kind = INT2;
        return token;
      }
    }

    token.kind = IDENTIFIER;
    return token;
  }

  if (lastChar == '\'') {
    while (peekChar(state) != '\'') {
      i32 next = nextChar(state);
      if (next == '\\') {
        nextChar(state);
      }
    }
    token.data = tokenStart;
    nextChar(state); // eat closing '
    token.end = state->current;
    token.kind = CONSTANT;
    return token;
  }

  if (lastChar == '"') {
    while (peekChar(state) != '"') {
      i32 next = nextChar(state);
      if (next == '\\') {
        nextChar(state);
      }
    }
    token.data = tokenStart + 1; // eat the starting "
    token.end = state->current;

    nextChar(state); // eat closing "
    token.kind = STRING_LITERAL;
    return token;
  }

  if (is_digit(lastChar) || (lastChar == '-' && is_digit(peekChar(state)))) {
    while (is_digit(peekChar(state)) || peekChar(state) == '.') {
      nextChar(state);
    }
    token.data = tokenStart;
    token.end = state->current;
    token.kind = CONSTANT;
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
  for (i32 i = CONTINUE; i < tokenSize; i++) {
    i64 len = strlen(tokens[i]) as i64;
    i64 remaining = state->end - tokenStart;
    if (len < remaining && memcmp(tokenStart, tokens[i], len as u64) == 0) {
      token.kind = i;
      token.data = tokenStart;

      state->current = tokenStart + len;
      token.end = state->current;

      return token;
    }
  }

  printf("Unknown token! %x\n", lastChar);
  failParse(state, "Unknown token");
  token.kind = TOK_EOF;
  return token;
}

i32 match(struct ParseState *state, i32 tok) {
  return state->curToken.kind == tok;
}

void expect(struct ParseState *state, i32 tok) {
  if (!match(state, tok)) {
    failParseArg(state, "Expected: ", tokens[tok]);
  }
}

struct Token getNextToken(struct ParseState *state) {
  struct Token result = state->curToken;
  state->curToken = getToken(state);
  return result;
}

struct ExprAST *parseNumber(struct ParseState *state) {
  struct ExprAST *result = newExpr(INT_EXPR);

  i8 *start = state->curToken.data;
  if (*start == '\'') {
    if (start[1] == '\\') {
      result->value = getEscaped(start[2]) as i32;
    } else {
      result->value = start[1] as i32;
    }
  } else {
    i8 *endp = state->curToken.end;
    i32 num = strtol(start, &endp, 10) as i32;
    result->value = num;
  }

  getNextToken(state);
  return result;
}

struct ExprAST *parseString(struct ParseState *state) {
  struct ExprAST *result = newExpr(STR_EXPR);
  result->identifier = state->curToken;
  getNextToken(state);
  return result;
}

struct ExprAST *parseIdentifier(struct ParseState *state) {
  struct ExprAST *result = newExpr(VARIABLE_EXPR);
  result->identifier = state->curToken;
  getNextToken(state);
  return result;
}

struct ExprAST *parseExpression(struct ParseState *state);

struct ExprAST *parseParen(struct ParseState *state) {
  getNextToken(state); // eat (

  struct ExprAST *expr = parseExpression(state);

  expect(state, CLOSE_PAREN);
  getNextToken(state); // eat )
  return expr;
}

struct ExprAST *parsePrimary(struct ParseState *state) {
  switch (state->curToken.kind) {
  case IDENTIFIER:
    return parseIdentifier(state);
  case CONSTANT:
    return parseNumber(state);
  case STRING_LITERAL:
    return parseString(state);
  case OPEN_PAREN:
    return parseParen(state);
  default:
    failParse(state, "Unknow primary expression");
    return NULL;
  }
}

struct ExprAST *parseIndex(struct ParseState *state, struct ExprAST *lhs) {
  struct ExprAST *expr = newExpr(INDEX_EXPR);
  expr->lhs = lhs;

  expr->rhs = parseExpression(state);

  expect(state, CLOSE_BRACKET);
  getNextToken(state);

  return expr;
}

struct ExprAST *parseAssignment(struct ParseState *state);

struct ExprAST *parseCall(struct ParseState *state, struct ExprAST *lhs) {
  struct ExprAST *expr = newExpr(CALL_EXPR);

  expr->lhs = lhs;
  expr->rhs = NULL;

  if (match(state, CLOSE_PAREN)) {
    getNextToken(state);
    return expr;
  }

  struct ExprAST *cur = expr;
  while (1) {
    cur->rhs = newExpr(ARG_LIST);
    cur = cur->rhs;

    cur->lhs = parseAssignment(state);
    cur->rhs = NULL;

    if (!match(state, COMMA)) {
      break;
    }
    getNextToken(state);
  }

  expect(state, CLOSE_PAREN);
  getNextToken(state);

  return expr;
}

struct ExprAST *parseMember(struct ParseState *state, struct ExprAST *lhs) {
  struct ExprAST *expr = newExpr(MEMBER_EXPR);
  expr->lhs = lhs;

  expr->op = state->curToken;
  getNextToken(state);

  expect(state, IDENTIFIER);
  expr->identifier = state->curToken;
  getNextToken(state);
  return expr;
}

struct ExprAST *parseUnaryPostfix(struct ParseState *state,
                                  struct ExprAST *lhs) {
  struct ExprAST *expr = newExpr(UNARY_EXPR);
  expr->lhs = lhs;
  expr->rhs = NULL;

  expr->op = state->curToken;
  getNextToken(state);

  return expr;
}

struct ExprAST *parsePostfix(struct ParseState *state) {
  struct ExprAST *expr = parsePrimary(state);

  while (1) {
    if (expr == NULL) {
      return expr;
    }

    switch (state->curToken.kind) {
    case OPEN_BRACKET:
      getNextToken(state);
      expr = parseIndex(state, expr);
      break;
    case OPEN_PAREN:
      getNextToken(state);
      expr = parseCall(state, expr);
      break;
    case DOT:
    case PTR_OP:
      // parseMember calls getNextToken after storing op.
      expr = parseMember(state, expr);
      break;
    case INC_OP:
    case DEC_OP:
      // parseMember calls getNextToken after storing op.
      expr = parseUnaryPostfix(state, expr);
      break;
    default:
      return expr;
    }
  }
  return expr;
}

i32 isDecl(struct Token tok) {
  // We don't support typedef, so this is easy
  switch (tok.kind) {
  case CONST:
  case STRUCT:
  case ENUM:
  case VOID:
  case INT2:
    return 1;
  default:
    return 0;
  }
}

void parseDeclSpecifier(struct ParseState *state, struct DeclAST *decl);

struct ExprAST *parseUnary(struct ParseState *state) {
  if (match(state, INC_OP) || match(state, DEC_OP) || match(state, AND) ||
      match(state, STAR) || match(state, PLUS) || match(state, MINUS) ||
      match(state, TILDE) || match(state, BANG)) {
    struct ExprAST *expr = newExpr(UNARY_EXPR);
    expr->op = state->curToken;
    getNextToken(state);
    expr->lhs = NULL;
    expr->rhs = parseUnary(state);
    return expr;
  }

  if (match(state, SIZEOF)) {
    getNextToken(state);
    struct ExprAST *expr = newExpr(SIZEOF_EXPR);

    expect(state, OPEN_PAREN);
    getNextToken(state);

    if (isDecl(state->curToken)) {
      struct DeclAST *dummy = newDecl();
      parseDeclSpecifier(state, dummy);
      expr->sizeofArg = dummy->type;
    } else {
      expr->lhs = NULL;
      expr->rhs = parseUnary(state);
    }

    expect(state, CLOSE_PAREN);
    getNextToken(state);

    return expr;
  }

  return parsePostfix(state);
}

struct ExprAST *parseCast(struct ParseState *state) {
  struct ExprAST *lhs = parseUnary(state);

  if (!match(state, AS)) {
    return lhs;
  }
  getNextToken(state);

  struct DeclAST *dummy = newDecl();
  parseDeclSpecifier(state, dummy);

  struct ExprAST *expr = newExpr(CAST_EXPR);
  expr->lhs = lhs;
  expr->type = dummy->type;
  return expr;
}

i32 getPrecedence(struct Token tok) {
  switch (tok.kind) {
  case STAR:
  case SLASH:
  case PERCENT:
    return 100;

  case PLUS:
  case MINUS:
    return 90;

  case LEFT_OP:
  case RIGHT_OP:
    return 80;

  case LESS:
  case GREATER:
  case LE_OP:
  case GE_OP:
    return 70;

  case EQ_OP:
  case NE_OP:
    return 60;

  case AND:
    return 50;
  case HAT:
    return 40;
  case PIPE:
    return 30;

  case AND_OP:
    return 20;
  case OR_OP:
    return 10;

  default:
    return -1;
  }
}

struct ExprAST *parseBinOpRhs(struct ParseState *state, i32 prec,
                              struct ExprAST *lhs) {
  while (1) {
    i32 curPred = getPrecedence(state->curToken);
    if (curPred < prec) {
      return lhs;
    }

    struct Token op = state->curToken;
    getNextToken(state);

    struct ExprAST *rhs = parseCast(state);

    i32 nextPred = getPrecedence(state->curToken);
    if (curPred < nextPred) {
      rhs = parseBinOpRhs(state, curPred + 1, rhs);
    }

    struct ExprAST *newLhs = newExpr(BINARY_EXPR);
    newLhs->op = op;
    newLhs->lhs = lhs;
    newLhs->rhs = rhs;

    lhs = newLhs;
  }
}

struct ExprAST *parseBinOp(struct ParseState *state) {
  struct ExprAST *lhs = parseCast(state);
  return parseBinOpRhs(state, 0, lhs);
}

struct ExprAST *parseConditional(struct ParseState *state) {
  struct ExprAST *cond = parseBinOp(state);
  if (!match(state, QUESTION)) {
    return cond;
  }
  getNextToken(state);

  struct ExprAST *trueBranch = parseExpression(state);
  expect(state, COLON);
  getNextToken(state);
  struct ExprAST *falseBranch = parseConditional(state);

  struct ExprAST *expr = newExpr(CONDITIONAL_EXPR);
  expr->cond = cond;
  expr->lhs = trueBranch;
  expr->rhs = falseBranch;
  return expr;
}



struct ExprAST *parseAssignment(struct ParseState *state) {
  struct ExprAST *lhs = parseConditional(state);
  if (!isAssign(state->curToken)) {
    return lhs;
  }

  struct Token op = getNextToken(state);
  struct ExprAST *rhs = parseAssignment(state);
  struct ExprAST *expr = newExpr(BINARY_EXPR);
  expr->op = op;
  expr->lhs = lhs;
  expr->rhs = rhs;
  return expr;
}

struct ExprAST *parseExpression(struct ParseState *state) {
  struct ExprAST *expr = parseAssignment(state);
  if (expr == NULL) {
    return expr;
  }
  while (match(state, COMMA)) {
    struct Token op = getNextToken(state);
    struct ExprAST *rhs = parseAssignment(state);
    if (rhs == NULL) {
      return expr;
    }

    struct ExprAST *new = newExpr(BINARY_EXPR);
    new->lhs = expr;
    new->op = op;
    new->rhs = rhs;

    expr = new;
  }
  return expr;
}

struct DeclAST *parseNoInitDecl(struct ParseState *state);

void parseStruct(struct ParseState *state, struct DeclAST *decl) {
  getNextToken(state); // eat struct
  decl->type = newType(STRUCT_TYPE);

  // (non)optional tag
  expect(state, IDENTIFIER);

  decl->type->tag = getNextToken(state);

  // Just a 'struct Foo' ref.
  if (!match(state, OPEN_BRACE)) {
    return;
  }
  getNextToken(state); // eat {

  decl->kind = STRUCT_DECL;

  // parse the fields
  struct DeclAST *fields = decl;
  while (!match(state, CLOSE_BRACE)) {
    fields->next = parseNoInitDecl(state);
    expect(state, SEMICOLON);
    getNextToken(state); // eat ;
    fields = fields->next;
  }
  getNextToken(state); // eat }

  fields->next = NULL;
  decl->fields = decl->next;
  decl->next = NULL;
}

void parseEnum(struct ParseState *state, struct DeclAST *decl) {
  getNextToken(state);
  decl->type = getInt32();

  // not supported: enum tag.
  expect(state, OPEN_BRACE);
  getNextToken(state);

  decl->kind = ENUM_DECL;

  // parse constants
  struct DeclAST *fields = decl;
  i32 idx = 0;
  while (!match(state, CLOSE_BRACE)) {
    expect(state, IDENTIFIER);

    struct DeclAST *field = newDecl();
    field->kind = ENUM_FIELD_DECL;
    field->type = getInt32();

    field->name = getNextToken(state);
    field->enumValue = idx++;

    fields->next = field;
    fields = field;

    if (match(state, CLOSE_BRACE)) {
      break;
    }

    expect(state, COMMA);
    getNextToken(state);
  }
  getNextToken(state); // eat }

  fields->next = NULL;
  decl->fields = decl->next;
  decl->next = NULL;
}

void parseDeclSpecifier(struct ParseState *state, struct DeclAST *decl) {
  i32 isConst = 0;
  if (match(state, CONST)) {
    getNextToken(state);
    isConst = 1;
  }

  // struct type ref or decl.
  if (match(state, STRUCT)) {
    parseStruct(state, decl);
  } else if (match(state, ENUM)) {
    parseEnum(state, decl);
  } else if (match(state, INT2)) {
    decl->type = newType(INT_TYPE2);
    decl->type->isSigned = *state->curToken.data == 'i';
    i8 *end = state->curToken.end;
    decl->type->size = strtol(state->curToken.data + 1, &end, 10) as i32;

    getNextToken(state);
  } else if (match(state, VOID)) {
    getNextToken(state);
    decl->type = newType(VOID_TYPE);
  } else {
    failParse(state, "Unknown type!");
  }
  decl->type->isConst = isConst;
}

void parseFuncDecl(struct ParseState *state, struct DeclAST *decl) {
  getNextToken(state); // eat (
  decl->kind = FUNC_DECL;

  struct Type *funcType = newType(FUNC_TYPE);
  funcType->result = decl->type;
  decl->type = funcType;

  if (match(state, CLOSE_PAREN)) {
    getNextToken(state);
    return;
  }

  // Parse args
  struct Type *curType = funcType;
  struct DeclAST *curDecl = decl;
  while (1) {
    if (match(state, ELLIPSIS)) {
      getNextToken(state);

      funcType->isVarargs = 1;

      expect(state, CLOSE_PAREN);
      getNextToken(state);
      break;
    }

    struct DeclAST *param = parseNoInitDecl(state);
    curDecl->next = param;
    curDecl = param;
    curType->argNext = param->type;
    curType = param->type;

    if (match(state, CLOSE_PAREN)) {
      getNextToken(state); // eat )
      break;
    }

    expect(state, COMMA);
    getNextToken(state); // eat ,
  }

  decl->fields = decl->next;
  decl->next = NULL;
  funcType->arg = funcType->argNext;
  funcType->argNext = NULL;
}

// simplified declarator
void parseDeclarator(struct ParseState *state, struct DeclAST *decl) {
  //  Parse pointers
  while (match(state, STAR)) {
    getNextToken(state);
    struct Type *ptrType = newType(POINTER_TYPE);
    ptrType->arg = decl->type;
    decl->type = ptrType;
  }

  expect(state, IDENTIFIER);
  decl->name = getNextToken(state);

  // (1D) arrays
  if (match(state, OPEN_BRACKET)) {
    getNextToken(state);

    // size not supported
    expect(state, CLOSE_BRACKET);
    getNextToken(state);

    struct Type *arrayType = newType(ARRAY_TYPE);
    arrayType->arg = decl->type;
    decl->type = arrayType;
  }
  // Function decl
  else if (match(state, OPEN_PAREN)) {
    parseFuncDecl(state, decl);
  }
}

struct DeclAST *parseNoInitDecl(struct ParseState *state) {
  struct DeclAST *decl = newDecl();
  decl->kind = VAR_DECL;

  // specifiers
  parseDeclSpecifier(state, decl);

  // We don't support taging and creating a struct in the same decl.
  if (decl->kind == STRUCT_DECL) {
    expect(state, SEMICOLON);
    return decl;
  }

  parseDeclarator(state, decl);
  return decl;
}

struct ExprAST *parseInitializer(struct ParseState *state) {
  if (!match(state, OPEN_BRACE)) {
    return parseAssignment(state);
  }
  getNextToken(state);

  struct ExprAST *expr = newExpr(ARRAY_EXPR);
  struct ExprAST *cur = expr;
  while (1) {
    // Should be parseInitializer(state), but let's not support nested inits.
    cur->lhs = parseAssignment(state);

    // close without trailing comma
    if (match(state, CLOSE_BRACE)) {
      break;
    }

    expect(state, COMMA);
    getNextToken(state); // eat ,

    // close with trailing comma
    if (match(state, CLOSE_BRACE)) {
      break;
    }

    cur->rhs = newExpr(ARRAY_EXPR);
    cur = cur->rhs;
  }
  getNextToken(state); // eat }
  cur->rhs = NULL;

  return expr;
}

struct StmtAST *newStmt(i32 kind) {
  struct StmtAST *stmt = calloc(1, sizeof(struct StmtAST));
  stmt->kind = kind;
  return stmt;
}

struct StmtAST *parseStmt(struct ParseState *state);

struct StmtAST *parseCompoundStmt(struct ParseState *state) {
  getNextToken(state); // eat {

  struct StmtAST *stmt = newStmt(COMPOUND_STMT);

  struct StmtAST *cur = stmt;
  while (!match(state, CLOSE_BRACE)) {
    cur->nextStmt = parseStmt(state);
    cur = cur->nextStmt;
  }
  getNextToken(state); // eat }

  stmt->stmt = stmt->nextStmt;
  stmt->nextStmt = NULL;
  return stmt;
}

struct StmtAST *parseExprStmt(struct ParseState *state) {
  struct StmtAST *stmt = newStmt(EXPR_STMT);

  if (!match(state, SEMICOLON)) {
    stmt->expr = parseExpression(state);
  }

  expect(state, SEMICOLON);
  getNextToken(state);

  return stmt;
}

struct DeclAST *parseDeclarationOrFunction(struct ParseState *state);

struct StmtAST *parseDeclStmt(struct ParseState *state) {
  struct StmtAST *stmt = newStmt(DECL_STMT);
  stmt->decl = parseDeclarationOrFunction(state);
  return stmt;
}

struct StmtAST *parseForStmt(struct ParseState *state) {
  getNextToken(state); // eat for
  struct StmtAST *stmt = newStmt(FOR_STMT);

  expect(state, OPEN_PAREN);
  getNextToken(state);

  if (isDecl(state->curToken)) {
    stmt->init = parseDeclStmt(state);
  } else {
    stmt->init = parseExprStmt(state);
  }
  stmt->cond = parseExprStmt(state);
  stmt->expr = parseExpression(state);

  expect(state, CLOSE_PAREN);
  getNextToken(state);

  stmt->stmt = parseStmt(state);

  return stmt;
}

struct StmtAST *parseIfStmt(struct ParseState *state) {
  getNextToken(state); // eat if

  expect(state, OPEN_PAREN);
  getNextToken(state);

  struct StmtAST *stmt = newStmt(IF_STMT);

  stmt->expr = parseExpression(state);
  expect(state, CLOSE_PAREN);
  getNextToken(state);

  stmt->init = parseStmt(state);

  if (match(state, ELSE)) {
    getNextToken(state);
    stmt->stmt = parseStmt(state);
  }

  return stmt;
}

struct StmtAST *parseReturnStmt(struct ParseState *state) {
  getNextToken(state);

  struct StmtAST *stmt = newStmt(RETURN_STMT);
  // parse value
  if (!match(state, SEMICOLON)) {
    stmt->expr = parseExpression(state);
  }

  expect(state, SEMICOLON);
  getNextToken(state);
  return stmt;
}

struct StmtAST *parseCaseStmt(struct ParseState *state) {
  getNextToken(state);

  struct StmtAST *stmt = newStmt(CASE_STMT);

  stmt->expr = parseConditional(state);

  expect(state, COLON);
  getNextToken(state);
  return stmt;
}

struct StmtAST *parseSwitchOrWhileStmt(struct ParseState *state, i32 kind) {
  getNextToken(state);

  expect(state, OPEN_PAREN);
  getNextToken(state);

  struct StmtAST *stmt = newStmt(kind);

  stmt->expr = parseExpression(state);
  expect(state, CLOSE_PAREN);
  getNextToken(state);

  stmt->stmt = parseStmt(state);

  return stmt;
}

struct StmtAST *parseStmt(struct ParseState *state) {
  if (isDecl(state->curToken)) {
    return parseDeclStmt(state);
  }

  if (match(state, OPEN_BRACE)) {
    return parseCompoundStmt(state);
  }

  if (match(state, FOR)) {
    return parseForStmt(state);
  }

  if (match(state, IF)) {
    return parseIfStmt(state);
  }

  if (match(state, RETURN)) {
    return parseReturnStmt(state);
  }

  if (match(state, SWITCH)) {
    return parseSwitchOrWhileStmt(state, SWITCH_STMT);
  }
  if (match(state, WHILE)) {
    return parseSwitchOrWhileStmt(state, WHILE_STMT);
  }

  if (match(state, CASE)) {
    return parseCaseStmt(state);
  }

  if (match(state, BREAK)) {
    getNextToken(state);
    struct StmtAST *stmt = newStmt(BREAK_STMT);

    expect(state, SEMICOLON);
    getNextToken(state);

    return stmt;
  }

  if (match(state, DEFAULT)) {
    getNextToken(state);
    struct StmtAST *stmt = newStmt(DEFAULT_STMT);

    expect(state, COLON);
    getNextToken(state);

    return stmt;
  }

  return parseExprStmt(state);
}

struct DeclAST *parseDeclarationOrFunction(struct ParseState *state) {
  struct DeclAST *decl = parseNoInitDecl(state);

  if (decl->kind == FUNC_DECL && match(state, OPEN_BRACE)) {
    decl->body = parseCompoundStmt(state);
    return decl;
  }

  // init (optional)
  if (match(state, EQ)) {
    getNextToken(state);

    decl->init = parseInitializer(state);
  }

  expect(state, SEMICOLON);
  getNextToken(state);

  return decl;
}

struct DeclAST *parseImportDecl(struct ParseState *state) {
  getNextToken(state); // eat import

  expect(state, IDENTIFIER);
  struct Token name = getNextToken(state);

  expect(state, SEMICOLON);
  getNextToken(state);

  struct DeclAST *decl = newDecl();
  decl->kind = IMPORT_DECL;
  decl->name = name;
  return decl;
}

struct DeclAST *parseTopLevelDecl(struct ParseState *state) {
  if (match(state, IMPORT)) {
    return parseImportDecl(state);
  }
  return parseDeclarationOrFunction(state);
}

struct DeclAST *parseTopLevel(struct ParseState *state) {
  getNextToken(state); // Prep token parser

  struct DeclAST *lastDecl = NULL;
  struct DeclAST *firstDecl = NULL;

  while (state->curToken.kind != TOK_EOF) {
    struct DeclAST *decl = parseTopLevelDecl(state);

    if (lastDecl == NULL) {
      firstDecl = decl;
    } else {
      lastDecl->next = decl;
    }

    lastDecl = decl;
  }

  return firstDecl;
}

