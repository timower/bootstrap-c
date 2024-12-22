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
    for (i32 i = TokenKind::CONTINUE as i32; i < tokenSize; i++) {
      if (tokCmpStr(token, tokens[i])) {
        token.kind = i as enum TokenKind;
        return token;
      }
    }

    // i32 types [iu](8|16|32|64)
    for (i32 i = 0; i < intTypeSize; i++) {
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
      i32 next = nextChar(state);
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
      i32 next = nextChar(state);
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
  for (i32 i = TokenKind::CONTINUE as i32; i < tokenSize; i++) {
    i64 len = strlen(tokens[i]) as i64;
    i64 remaining = state->end - tokenStart;
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

i32 match(struct ParseState *state, enum TokenKind tok) {
  return state->curToken.kind == tok;
}

void expect(struct ParseState *state, enum TokenKind tok) {
  if (!match(state, tok)) {
    failParseArg(state, "Expected: ", tokens[tok as i32]);
  }
}

struct Token getNextToken(struct ParseState *state) {
  struct Token result = state->curToken;
  state->curToken = getToken(state);
  return result;
}

// number := [0-9]+ | '[\n\t\r\\'"]' | '.'
struct ExprAST *parseNumber(struct ParseState *state) {
  struct ExprAST *result = newExpr(ExprKind::INT);

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

// string := '"' [^"]* '"'
struct ExprAST *parseString(struct ParseState *state) {
  struct ExprAST *result = newExpr(ExprKind::STR);
  result->identifier = state->curToken;
  getNextToken(state);
  return result;
}

// ident_or_scope := identifier | identifier '::' identifier
struct ExprAST *parseIdentifierOrScope(struct ParseState *state) {
  struct Token ident = getNextToken(state);

  if (!match(state, TokenKind::SCOPE)) {
    struct ExprAST *result = newExpr(ExprKind::VARIABLE);
    result->identifier = ident;
    return result;
  }

  getNextToken(state); // eat ::
  expect(state, TokenKind::IDENTIFIER);

  struct ExprAST *result = newExpr(ExprKind::SCOPE);
  result->parent = ident;
  result->identifier = getNextToken(state);
  return result;
}

struct ExprAST *parseExpression(struct ParseState *state);

// paren := '(' expression ')'
struct ExprAST *parseParen(struct ParseState *state) {
  getNextToken(state); // eat (

  struct ExprAST *expr = parseExpression(state);

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state); // eat )
  return expr;
}

// primary := identifier
//          | identifier '::' identifier
//          | number
//          | string
//          | paren
struct ExprAST *parsePrimary(struct ParseState *state) {
  switch (state->curToken.kind) {
  case TokenKind::IDENTIFIER:
    return parseIdentifierOrScope(state);
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
struct ExprAST *parseIndex(struct ParseState *state, struct ExprAST *lhs) {
  getNextToken(state); // eat [

  struct ExprAST *expr = newExpr(ExprKind::INDEX);
  expr->lhs = lhs;

  expr->rhs = parseExpression(state);

  expect(state, TokenKind::CLOSE_BRACKET);
  getNextToken(state);

  return expr;
}

struct ExprAST *parseAssignment(struct ParseState *state);

// call := lhs '(' [assignment (',' assigment)*] ')'
struct ExprAST *parseCall(struct ParseState *state, struct ExprAST *lhs) {
  getNextToken(state); // eat (

  struct ExprAST *expr = newExpr(ExprKind::CALL);

  expr->lhs = lhs;
  expr->rhs = NULL;

  if (match(state, TokenKind::CLOSE_PAREN)) {
    getNextToken(state);
    return expr;
  }

  struct ExprAST *cur = expr;
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
struct ExprAST *parseMember(struct ParseState *state, struct ExprAST *lhs) {
  struct ExprAST *expr = newExpr(ExprKind::MEMBER);
  expr->lhs = lhs;

  expr->op = state->curToken;
  getNextToken(state);

  expect(state, TokenKind::IDENTIFIER);
  expr->identifier = state->curToken;
  getNextToken(state);
  return expr;
}

// unary_postfix := lhs '++' | lhs '--'
struct ExprAST *parseUnaryPostfix(struct ParseState *state,
                                  struct ExprAST *lhs) {
  struct ExprAST *expr = newExpr(ExprKind::UNARY);
  expr->lhs = lhs;
  expr->rhs = NULL;

  expr->op = state->curToken;
  getNextToken(state);

  return expr;
}

// postfix := primary ( [index | call | member | unary_postfix] )*
struct ExprAST *parsePostfix(struct ParseState *state) {
  struct ExprAST *expr = parsePrimary(state);

  while (1) {
    if (expr == NULL) {
      return expr;
    }

    switch (state->curToken.kind) {
    case TokenKind::OPEN_BRACKET:
      expr = parseIndex(state, expr);
      break;
    case TokenKind::OPEN_PAREN:
      expr = parseCall(state, expr);
      break;
    case TokenKind::DOT:
    case TokenKind::PTR_OP:
      expr = parseMember(state, expr);
      break;
    case TokenKind::INC_OP:
    case TokenKind::DEC_OP:
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
  case TokenKind::CONST:
  case TokenKind::STRUCT:
  case TokenKind::ENUM:
  case TokenKind::VOID:
  case TokenKind::INT2:
    return 1;
  default:
    return 0;
  }
}

i32 isUnary(struct Token tok) {
  switch (tok.kind) {
  case TokenKind::INC_OP:
  case TokenKind::DEC_OP:
  case TokenKind::AND:
  case TokenKind::STAR:
  case TokenKind::PLUS:
  case TokenKind::MINUS:
  case TokenKind::TILDE:
  case TokenKind::BANG:
    return 1;
  default:
    return 0;
  }
}

void parseDeclSpecifier(struct ParseState *state, struct DeclAST *decl);

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
struct ExprAST *parseUnary(struct ParseState *state) {
  if (isUnary(state->curToken)) {
    struct ExprAST *expr = newExpr(ExprKind::UNARY);
    expr->op = state->curToken;
    getNextToken(state);
    expr->lhs = NULL;
    expr->rhs = parseUnary(state);
    return expr;
  }

  if (match(state, TokenKind::SIZEOF)) {
    getNextToken(state);
    struct ExprAST *expr = newExpr(ExprKind::SIZEOF);

    expect(state, TokenKind::OPEN_PAREN);
    getNextToken(state);

    if (isDecl(state->curToken)) {
      struct DeclAST *dummy = newDecl();
      parseDeclSpecifier(state, dummy);
      expr->sizeofArg = dummy->type;
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
struct ExprAST *parseCast(struct ParseState *state) {
  struct ExprAST *lhs = parseUnary(state);

  if (!match(state, TokenKind::AS)) {
    return lhs;
  }
  getNextToken(state);

  struct DeclAST *dummy = newDecl();
  parseDeclSpecifier(state, dummy);

  struct ExprAST *expr = newExpr(ExprKind::CAST);
  expr->lhs = lhs;
  expr->type = dummy->type;
  return expr;
}

i32 getPrecedence(struct Token tok) {
  switch (tok.kind) {
  case TokenKind::STAR:
  case TokenKind::SLASH:
  case TokenKind::PERCENT:
    return 100;

  case TokenKind::PLUS:
  case TokenKind::MINUS:
    return 90;

  case TokenKind::LEFT_OP:
  case TokenKind::RIGHT_OP:
    return 80;

  case TokenKind::LESS:
  case TokenKind::GREATER:
  case TokenKind::LE_OP:
  case TokenKind::GE_OP:
    return 70;

  case TokenKind::EQ_OP:
  case TokenKind::NE_OP:
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

    struct ExprAST *newLhs = newExpr(ExprKind::BINARY);
    newLhs->op = op;
    newLhs->lhs = lhs;
    newLhs->rhs = rhs;

    lhs = newLhs;
  }
}

// binary := cast (op cast)*
struct ExprAST *parseBinOp(struct ParseState *state) {
  struct ExprAST *lhs = parseCast(state);
  return parseBinOpRhs(state, 0, lhs);
}

// conditional := binary
//              | binary '?' expression ':' conditional
struct ExprAST *parseConditional(struct ParseState *state) {
  struct ExprAST *cond = parseBinOp(state);
  if (!match(state, TokenKind::QUESTION)) {
    return cond;
  }
  getNextToken(state);

  struct ExprAST *trueBranch = parseExpression(state);
  expect(state, TokenKind::COLON);
  getNextToken(state);
  struct ExprAST *falseBranch = parseConditional(state);

  struct ExprAST *expr = newExpr(ExprKind::CONDITIONAL);
  expr->cond = cond;
  expr->lhs = trueBranch;
  expr->rhs = falseBranch;
  return expr;
}

// assignment := conditional | conditional '=' assignment
struct ExprAST *parseAssignment(struct ParseState *state) {
  struct ExprAST *lhs = parseConditional(state);
  if (!isAssign(state->curToken)) {
    return lhs;
  }

  struct Token op = getNextToken(state);
  struct ExprAST *rhs = parseAssignment(state);
  struct ExprAST *expr = newExpr(ExprKind::BINARY);
  expr->op = op;
  expr->lhs = lhs;
  expr->rhs = rhs;
  return expr;
}

// expression := assignment (',' assignment)*
struct ExprAST *parseExpression(struct ParseState *state) {
  struct ExprAST *expr = parseAssignment(state);
  if (expr == NULL) {
    return expr;
  }
  while (match(state, TokenKind::COMMA)) {
    struct Token op = getNextToken(state);
    struct ExprAST *rhs = parseAssignment(state);
    if (rhs == NULL) {
      return expr;
    }

    struct ExprAST *new = newExpr(ExprKind::BINARY);
    new->lhs = expr;
    new->op = op;
    new->rhs = rhs;

    expr = new;
  }
  return expr;
}

struct DeclAST *parseNoInitDecl(struct ParseState *state);

// struct := 'struct' identifier '{' decl* '}'
//         | 'struct' identifier
void parseStruct(struct ParseState *state, struct DeclAST *decl) {
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
  struct DeclAST *fields = decl;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    fields->next = parseNoInitDecl(state);
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
void parseEnum(struct ParseState *state, struct DeclAST *decl) {
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
  struct DeclAST *fields = decl;
  i32 idx = 0;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    expect(state, TokenKind::IDENTIFIER);

    struct DeclAST *field = newDecl();
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

// decl_specifier := ['const'] ( struct | enum | int | 'void' )
void parseDeclSpecifier(struct ParseState *state, struct DeclAST *decl) {
  i32 isConst = 0;
  if (match(state, TokenKind::CONST)) {
    getNextToken(state);
    isConst = 1;
  }

  // struct type ref or decl.
  if (match(state, TokenKind::STRUCT)) {
    parseStruct(state, decl);
  } else if (match(state, TokenKind::ENUM)) {
    parseEnum(state, decl);
  } else if (match(state, TokenKind::INT2)) {
    decl->type = newType(TypeKind::INT);
    decl->type->isSigned = *state->curToken.data == 'i';
    i8 *end = state->curToken.end;
    decl->type->size = strtol(state->curToken.data + 1, &end, 10) as i32;

    getNextToken(state);
  } else if (match(state, TokenKind::VOID)) {
    getNextToken(state);
    decl->type = newType(TypeKind::VOID);
  } else {
    failParse(state, "Unknown type!");
  }
  decl->type->isConst = isConst;
}

// func_decl := '(' [decl (',' decl)*] ')'
void parseFuncDecl(struct ParseState *state, struct DeclAST *decl) {
  getNextToken(state); // eat (
  decl->kind = DeclKind::FUNC;

  struct Type *funcType = newType(TypeKind::FUNC);
  funcType->result = decl->type;
  decl->type = funcType;

  if (match(state, TokenKind::CLOSE_PAREN)) {
    getNextToken(state);
    return;
  }

  // Parse args
  struct Type *curType = funcType;
  struct DeclAST *curDecl = decl;
  while (1) {
    if (match(state, TokenKind::ELLIPSIS)) {
      getNextToken(state);

      funcType->isVarargs = 1;

      expect(state, TokenKind::CLOSE_PAREN);
      getNextToken(state);
      break;
    }

    struct DeclAST *param = parseNoInitDecl(state);
    curDecl->next = param;
    curDecl = param;
    curType->argNext = param->type;
    curType = param->type;

    if (match(state, TokenKind::CLOSE_PAREN)) {
      getNextToken(state); // eat )
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state); // eat ,
  }

  decl->fields = decl->next;
  decl->next = NULL;
  funcType->arg = funcType->argNext;
  funcType->argNext = NULL;
}

// declarator := star* identifier ('[' ']' | func_decl )
void parseDeclarator(struct ParseState *state, struct DeclAST *decl) {
  //  Parse pointers
  while (match(state, TokenKind::STAR)) {
    getNextToken(state);
    struct Type *ptrType = newType(TypeKind::POINTER);
    ptrType->arg = decl->type;
    decl->type = ptrType;
  }

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  // (1D) arrays
  if (match(state, TokenKind::OPEN_BRACKET)) {
    getNextToken(state);

    // size not supported
    expect(state, TokenKind::CLOSE_BRACKET);
    getNextToken(state);

    struct Type *arrayType = newType(TypeKind::ARRAY);
    arrayType->arg = decl->type;
    decl->type = arrayType;
  } else if (match(state, TokenKind::OPEN_PAREN)) {
    parseFuncDecl(state, decl);
  }
}

// no_init_decl := decl_specifier declarator
struct DeclAST *parseNoInitDecl(struct ParseState *state) {
  struct DeclAST *decl = newDecl();
  decl->kind = DeclKind::VAR;

  // specifiers
  parseDeclSpecifier(state, decl);

  // We don't support taging and creating a struct or enum in the same decl.
  if (decl->kind == DeclKind::STRUCT || decl->kind == DeclKind::ENUM) {
    expect(state, TokenKind::SEMICOLON);
    return decl;
  }

  parseDeclarator(state, decl);
  return decl;
}

// initializer := assignment | '{' assignment (',' assignment)* ','? '}'
struct ExprAST *parseInitializer(struct ParseState *state) {
  if (!match(state, TokenKind::OPEN_BRACE)) {
    return parseAssignment(state);
  }
  getNextToken(state);

  struct ExprAST *expr = newExpr(ExprKind::ARRAY);
  struct ExprAST *cur = expr;
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

struct StmtAST *parseStmt(struct ParseState *state);

struct StmtAST *parseCompoundStmt(struct ParseState *state) {
  getNextToken(state); // eat {

  struct StmtAST *stmt = newStmt(StmtKind::COMPOUND);

  struct StmtAST *cur = stmt;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    cur->nextStmt = parseStmt(state);
    cur = cur->nextStmt;
  }
  getNextToken(state); // eat }

  stmt->stmt = stmt->nextStmt;
  stmt->nextStmt = NULL;
  return stmt;
}

struct StmtAST *parseExprStmt(struct ParseState *state) {
  struct StmtAST *stmt = newStmt(StmtKind::EXPR);

  if (!match(state, TokenKind::SEMICOLON)) {
    stmt->expr = parseExpression(state);
  }

  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);

  return stmt;
}

struct DeclAST *parseDeclarationOrFunction(struct ParseState *state);

struct StmtAST *parseDeclStmt(struct ParseState *state) {
  struct StmtAST *stmt = newStmt(StmtKind::DECL);
  stmt->decl = parseDeclarationOrFunction(state);
  return stmt;
}

struct StmtAST *parseForStmt(struct ParseState *state) {
  getNextToken(state); // eat for
  struct StmtAST *stmt = newStmt(StmtKind::FOR);

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

struct StmtAST *parseIfStmt(struct ParseState *state) {
  getNextToken(state); // eat if

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  struct StmtAST *stmt = newStmt(StmtKind::IF);

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

struct StmtAST *parseReturnStmt(struct ParseState *state) {
  getNextToken(state);

  struct StmtAST *stmt = newStmt(StmtKind::RETURN);
  // parse value
  if (!match(state, TokenKind::SEMICOLON)) {
    stmt->expr = parseExpression(state);
  }

  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);
  return stmt;
}

struct StmtAST *parseCaseStmt(struct ParseState *state) {
  getNextToken(state);

  struct StmtAST *stmt = newStmt(StmtKind::CASE);

  stmt->expr = parseConditional(state);

  expect(state, TokenKind::COLON);
  getNextToken(state);
  return stmt;
}

struct StmtAST *parseSwitchOrWhileStmt(struct ParseState *state,
                                       enum StmtKind kind) {
  getNextToken(state);

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  struct StmtAST *stmt = newStmt(kind);

  stmt->expr = parseExpression(state);
  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  stmt->stmt = parseStmt(state);

  return stmt;
}

struct StmtAST *parseStmt(struct ParseState *state) {
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
    return parseSwitchOrWhileStmt(state, StmtKind::SWITCH);
  }
  if (match(state, TokenKind::WHILE)) {
    return parseSwitchOrWhileStmt(state, StmtKind::WHILE);
  }

  if (match(state, TokenKind::CASE)) {
    return parseCaseStmt(state);
  }

  if (match(state, TokenKind::BREAK)) {
    getNextToken(state);
    struct StmtAST *stmt = newStmt(StmtKind::BREAK);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    return stmt;
  }

  if (match(state, TokenKind::DEFAULT)) {
    getNextToken(state);
    struct StmtAST *stmt = newStmt(StmtKind::DEFAULT);

    expect(state, TokenKind::COLON);
    getNextToken(state);

    return stmt;
  }

  return parseExprStmt(state);
}

struct DeclAST *parseDeclarationOrFunction(struct ParseState *state) {
  struct DeclAST *decl = parseNoInitDecl(state);

  if (decl->kind == DeclKind::FUNC && match(state, TokenKind::OPEN_BRACE)) {
    decl->body = parseCompoundStmt(state);
    return decl;
  }

  // init (optional)
  if (match(state, TokenKind::EQ)) {
    getNextToken(state);

    decl->init = parseInitializer(state);
  }

  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);

  return decl;
}

struct DeclAST *parseImportDecl(struct ParseState *state) {
  getNextToken(state); // eat import

  expect(state, TokenKind::IDENTIFIER);
  struct Token name = getNextToken(state);

  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);

  struct DeclAST *decl = newDecl();
  decl->kind = DeclKind::IMPORT;
  decl->name = name;
  return decl;
}

struct DeclAST *parseTopLevelDecl(struct ParseState *state) {
  if (match(state, TokenKind::IMPORT)) {
    return parseImportDecl(state);
  }
  return parseDeclarationOrFunction(state);
}

struct DeclAST *parseTopLevel(struct ParseState *state) {
  getNextToken(state); // Prep token parser

  struct DeclAST *lastDecl = NULL;
  struct DeclAST *firstDecl = NULL;

  while (state->curToken.kind != TokenKind::TOK_EOF) {
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
