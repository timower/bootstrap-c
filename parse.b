import ast;
import print_ast;
import util;

struct ParseOptions {
  // If set to true, build a concere syntax tree,
  // preserving parens.
  concrete: i32;
};

struct ParseState {
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

  options: ParseOptions;

  // Any comments that should be taken up by the next node.
  // Only parsed if concrete is true.
  comments: Comment*;
  lastComment: Comment*;
};


// 1. parse
let intTypes: const i8*[] = {
  "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64",
};

func iseol(c: i32) -> i32 {
  return c == 10 || c == 13;
}

func getLocation(state: ParseState*) -> SourceLoc {
  return SourceLoc {
    line = state->line,
    column = (state->current - state->lineStart) as i32,
    fileName = state->fileName,
  };
}


// Pops any comments on the given line from state.
func getLineComments(state: ParseState*, line: i32) -> Comment* {
  let firstComment = state->comments;
  if (firstComment == null || firstComment->location.line > line) {
    return null;
  }

  let lastComment = firstComment;
  while (lastComment->next != null
      && lastComment->next->location.line <= line) {
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
  printf("%s:%d:%d: ", state->fileName, location.line, location.column);
  if (state->curToken.data < state->end) {
    printToken(state->curToken);
  }
  printf(": %s%s\n", msg, arg);
  exit(1);
}

func failParse(state: ParseState*, msg: const i8*) {
  failParseArg(state, msg, "");
}


// Returns the current character and advances the current pointer.
func nextChar(state: ParseState*) -> i32 {
  if (state->current >= state->end) {
    return -1;
  }

  let result = *state->current as i32;
  state->current++;

  if (iseol(result)) {
    state->line++;
    state->lineStart = state->current;
  }

  return result;
}


// Returns the current character without advancing
func peekChar(state: ParseState*) -> i32 {
  if (state->current >= state->end) {
    return -1;
  }
  return *state->current as i32;
}


/// True if the current character is an EOL character
func is_space(c: i32) -> i32 {
  return iseol(c) || c == 32 || c == 9;
}

func is_alpha(c: i32) -> i32 {
  return (c >= 97 && c <= 122) || (c >= 65 && c <= 90);
}

func is_digit(c: i32) -> i32 {
  return c >= 48 && c <= 57;
}

func is_alnum(c: i32) -> i32 {
  return is_digit(c) || is_alpha(c);
}

func getToken(state: ParseState*) -> Token {
  let tokenStart = state->current;
  let lastChar = nextChar(state);

  let token = Token {};

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
  if (is_alpha(lastChar) || lastChar == 95) {
    while (is_alnum(peekChar(state)) || peekChar(state) == 95) {
      nextChar(state);
    }

    token.data = tokenStart;
    token.end = state->current;    // one past the end!

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

  if (lastChar == 39) {
    while (peekChar(state) != 39) {
      let next = nextChar(state);
      if (next == 92) {
        nextChar(state);
      }
    }
    token.data = tokenStart;
    nextChar(state);    // eat closing '
    token.end = state->current;
    token.kind = TokenKind::CONSTANT;
    return token;
  }

  if (lastChar == 34) {
    while (peekChar(state) != 34) {
      let next = nextChar(state);
      if (next == 92) {
        nextChar(state);
      }
    }
    token.data = tokenStart + 1;    // eat the starting "
    token.end = state->current;

    nextChar(state);    // eat closing "
    token.kind = TokenKind::STRING_LITERAL;
    return token;
  }

  if (is_digit(lastChar) || (lastChar == 45 && is_digit(peekChar(state)))) {
    while (is_digit(peekChar(state)) || peekChar(state) == 46) {
      nextChar(state);
    }
    token.data = tokenStart;
    token.end = state->current;
    token.kind = TokenKind::CONSTANT;
    return token;
  }

  // pre-processor
  if (lastChar == 35) {
    while (!iseol(peekChar(state))) {
      nextChar(state);
    }
    return getToken(state);
  }

  // Comments //
  if (lastChar == 47 && peekChar(state) == 47) {
    while (!iseol(peekChar(state))) {
      nextChar(state);
    }

    if (state->options.concrete) {
      token.kind = TokenKind::COMMENT;
      token.data = tokenStart;
      token.end = state->current;
      return token;
    } else {
      return getToken(state);
    }
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

func match(state: ParseState*, tok: TokenKind) -> i32 {
  return state->curToken.kind == tok;
}

func expect(state: ParseState*, tok: TokenKind) {
  if (!match(state, tok)) {
    failParseArg(state, "Expected: ", tokens[(tok as i32)]);
  }
}

func getNextToken(state: ParseState*) -> Token {
  let result = state->curToken;

  let token = getToken(state);
  while (token.kind == TokenKind::COMMENT) {
    let comment = newComment(token);
    comment->location = getLocation(state);

    if (state->lastComment != null) {
      state->lastComment->next = comment;
    }
    state->lastComment = comment;
    if (state->comments == null) {
      state->comments = comment;
    }

    token = getToken(state);
  }
  state->curToken = token;

  return result;
}

func newLocExpr(state: ParseState*, kind: ExprKind) -> ExprAST* {
  let res = newExpr(kind);
  res->location = getLocation(state);
  return res;
}


// number := [0-9]+ | '[\n\t\r\\'"]' | '.'
func parseNumber(state: ParseState*) -> ExprAST* {
  let result = newLocExpr(state, ExprKind::INT);
  result->op = state->curToken;

  let start = state->curToken.data;
  if (*start == 39) {
    let next = *(start + 1);
    if (next == 92) {
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
func parseString(state: ParseState*) -> ExprAST* {
  let result = newLocExpr(state, ExprKind::STR);
  result->identifier = state->curToken;
  getNextToken(state);
  return result;
}

func parseAssignment(state: ParseState*) -> ExprAST*;

func parseConditional(state: ParseState*) -> ExprAST*;


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

func parseExpression(state: ParseState*) -> ExprAST*;


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
  while (1) {
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

  while (1) {
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

func isDecl(tok: Token) -> i32 {
  // We don't support typedef, so this is easy
  switch (tok.kind) {
    case TokenKind::STRUCT,
         TokenKind::ENUM,
         TokenKind::LET,
         TokenKind::FUNC,
         TokenKind::UNION:
      return 1;
    default:
      return 0;
  }
}

func isUnary(tok: Token) -> i32 {
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
func parseType(state: ParseState*) -> Type* {
  let type = newType(TypeKind::Void {});

  if (match(state, TokenKind::CONST)) {
    getNextToken(state);
    type->isConst = 1;
  }

  if (match(state, TokenKind::INT2)) {
    let isSigned = *state->curToken.data == 105;
    let end = state->curToken.end;
    let size = strtol(state->curToken.data + 1, &end, 10) as i32;
    getNextToken(state);
    type->kind = TypeKind::Int {
      size = size,
      isSigned = isSigned,
    };
  } else if (match(state, TokenKind::VOID)) {
    getNextToken(state);
  } else if (match(state, TokenKind::STRUCT)) {
    getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);
    type->kind = TypeKind::Struct {
      tag = getNextToken(state),
    };
  } else if (match(state, TokenKind::ENUM)) {
    getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);
    type->kind = TypeKind::Enum {
      tag = getNextToken(state),
    };
  } else if (match(state, TokenKind::UNION)) {
    getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);
    type->kind = TypeKind::Union {
      tag = getNextToken(state),
    };
  } else if (match(state, TokenKind::IDENTIFIER)) {
    type->kind = TypeKind::Tag {
      tag = getNextToken(state),
    };
  } else {
    failParse(state, "Unknown type");
    return null;
  }

  if (match(state, TokenKind::SCOPE)) {
    getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);

    let tagPtr = &type->kind as TypeKind::Tag*;
    tagPtr->parent = tagPtr->tag;
    tagPtr->tag = getNextToken(state);
  }

  // parse type suffixes (pointers & arrays)
  while (1) {
    if (match(state, TokenKind::STAR)) {
      getNextToken(state);
      let ptrType = newType(TypeKind::Pointer {
        pointee = type,
      });
      type = ptrType;
    } else if (match(state, TokenKind::OPEN_BRACKET)) {
      getNextToken(state);

      let size = -1;
      if (match(state, TokenKind::CONSTANT)) {
        let numExpr = parseNumber(state);
        size = numExpr->value;
      }

      let arrayType = newType(TypeKind::Array {
        size = size,
        element = type,
      });

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
  while (1) {
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


func parseLetDecl(state: ParseState*) -> DeclAST*;


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


// Add any comments in state that are on the same line as decl to decl.
func addTrailingCommentsStmt(state: ParseState*, stmt: StmtAST*) {
  let comments = getLineComments(state, stmt->endLocation.line);
  if (comments == null) {
    return;
  }

  stmt->comments = appendComments(stmt->comments, comments);
}

func parseStmt(state: ParseState*) -> StmtAST*;

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

func parseDeclarationOrFunction(state: ParseState*) -> DeclAST*;

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


// struct DeclAST *parseNoInitDecl(state: ParseState*);
// initializer := assignment | '{' assignment (',' assignment)* ','? '}'
func parseInitializer(state: ParseState*) -> ExprAST* {
  if (!match(state, TokenKind::OPEN_BRACE)) {
    return parseAssignment(state);
  }

  let expr = newLocExpr(state, ExprKind::ARRAY);
  getNextToken(state);  // eat '{'

  let cur = expr;
  while (1) {
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


// Add any comments in state that are on the same line as decl to decl.
func addTrailingCommentsDecl(state: ParseState*, decl: DeclAST*) {
  let comments = getLineComments(state, decl->endLocation.line);
  if (comments == null) {
    return;
  }

  decl->comments = appendComments(decl->comments, comments);
}


// type_name_pair := identifier ':' type
func parseNameTypePair(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::VAR);

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  expect(state, TokenKind::COLON);
  getNextToken(state);

  decl->type = parseType(state);
  decl->endLocation = getLocation(state);

  return decl;
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

func parseSubStruct(state: ParseState*, decl: DeclAST*) {
  // (non)optional tag
  expect(state, TokenKind::IDENTIFIER);

  decl->type = newType(TypeKind::Struct {
    tag = getNextToken(state),
  });

  expect(state, TokenKind::OPEN_BRACE);
  getNextToken(state);  // eat {

  decl->kind = DeclKind::STRUCT;

  // parse the fields
  let fields = decl;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    fields->next = parseNameTypePair(state);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);    // eat ;

    addTrailingCommentsDecl(state, fields->next);

    fields = fields->next;
  }
  decl->endLocation = getLocation(state);
  getNextToken(state);  // eat }

  fields->next = null;
  decl->fields = decl->next;
  decl->next = null;
}


// struct := 'struct' identifier '{' decl* '}'
func parseStruct(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::STRUCT);
  getNextToken(state);  // eat struct
  parseSubStruct(state, decl);
  return decl;
}


// enum := 'enum' identifier '{' identifier  (',' identifier )* ','? '}'
func parseEnum(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::ENUM);
  getNextToken(state);

  expect(state, TokenKind::IDENTIFIER);

  decl->type = newType(TypeKind::Enum {
    tag = getNextToken(state),
  });

  expect(state, TokenKind::OPEN_BRACE);
  getNextToken(state);

  // parse constants
  let fields = decl;
  let idx = 0;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    expect(state, TokenKind::IDENTIFIER);

    let field = newLocDecl(state, DeclKind::ENUM_FIELD);
    field->type = getInt32();

    field->name = getNextToken(state);
    field->enumValue = idx++;

    fields->next = field;
    fields = field;

    field->endLocation = getLocation(state);

    if (match(state, TokenKind::CLOSE_BRACE)) {
      addTrailingCommentsDecl(state, field);
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state);

    addTrailingCommentsDecl(state, field);
  }
  decl->endLocation = getLocation(state);
  getNextToken(state);  // eat }

  fields->next = null;
  decl->fields = decl->next;
  decl->next = null;
  return decl;
}

func parseUnion(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::UNION);
  getNextToken(state);  // eat 'union'

  expect(state, TokenKind::IDENTIFIER);
  decl->type = newType(TypeKind::Union {
    tag = getNextToken(state),
  });

  expect(state, TokenKind::OPEN_BRACE);
  getNextToken(state);

  let declListPtr = &decl->subTypes;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    let tag = newLocDecl(state, DeclKind::STRUCT);
    parseSubStruct(state, tag);

    // Use 'arg' of the struct type to point to the parent type.
    let structType = &tag->type->kind as TypeKind::Struct*;
    structType->parent = decl->type;

    // TODO: trailing comments?
    let newList = newDeclList(tag);
    *declListPtr = newList;
    declListPtr = &newList->next;
  }

  decl->endLocation = getLocation(state);
  getNextToken(state);  // eat }

  return decl;
}


// func_decl :=
//  'func' identifier [ '->' type ] '(' [decl (',' decl)*] ')' compound_stmt?
func parseFuncDecl(state: ParseState*) -> DeclAST* {
  getNextToken(state);  // eat func

  let decl = newLocDecl(state, DeclKind::FUNC);

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  decl->type = newType(TypeKind::Func {});
  let funcType = &decl->type->kind as TypeKind::Func*;

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);  // eat (

  let curType = decl->type;
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
    curType->next = param->type;
    curType = param->type;

    if (match(state, TokenKind::CLOSE_PAREN)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state);    // eat ,
  }
  getNextToken(state);  // eat )

  decl->fields = decl->next;
  decl->next = null;

  funcType->args = decl->type->next;
  decl->type->next = null;

  if (match(state, TokenKind::PTR_OP)) {
    getNextToken(state);    // eat ->
    funcType->result = parseType(state);
  } else {
    funcType->result = newType(TypeKind::Void {});
  }

  if (match(state, TokenKind::OPEN_BRACE)) {
    decl->body = parseCompoundStmt(state);
    decl->endLocation = decl->body->endLocation;
  } else {
    expect(state, TokenKind::SEMICOLON);
    decl->endLocation = getLocation(state);
    getNextToken(state);    // eat ;
  }

  addTrailingCommentsDecl(state, decl);

  return decl;
}

func parseDeclarationOrFunction(state: ParseState*) -> DeclAST* {
  if (match(state, TokenKind::LET)) {
    let decl = parseLetDecl(state);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    addTrailingCommentsDecl(state, decl);
    return decl;
  }

  if (match(state, TokenKind::FUNC)) {
    return parseFuncDecl(state);
  }

  if (match(state, TokenKind::STRUCT)) {
    let decl = parseStruct(state);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    addTrailingCommentsDecl(state, decl);

    return decl;
  }

  if (match(state, TokenKind::ENUM)) {
    let decl = parseEnum(state);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    addTrailingCommentsDecl(state, decl);

    return decl;
  }

  if (match(state, TokenKind::UNION)) {
    let decl = parseUnion(state);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    addTrailingCommentsDecl(state, decl);

    return decl;
  }

  failParse(state, "Unknown declaration");
  return null;
}

func parseImportDecl(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::IMPORT);
  getNextToken(state);  // eat import

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  decl->endLocation = getLocation(state);
  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);

  addTrailingCommentsDecl(state, decl);

  return decl;
}

func parseTopLevelDecl(state: ParseState*) -> DeclAST* {
  if (match(state, TokenKind::IMPORT)) {
    return parseImportDecl(state);
  }
  return parseDeclarationOrFunction(state);
}

func parseTopLevel(state: ParseState*) -> DeclAST* {
  getNextToken(state);  // Prep token parser

  let lastDecl: DeclAST* = null;
  let firstDecl: DeclAST* = null;

  while (state->curToken.kind != TokenKind::TOK_EOF) {
    let decl = parseTopLevelDecl(state);

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
