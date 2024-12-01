#include <ctype.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// 1. parse
// 2. sema
// 3. emit

struct Token {
  enum {
    TOK_EOF,

    // clang-format off
    // constants
    IDENTIFIER, CONSTANT, STRING_LITERAL,
    // TYPE_NAME,

    // operators
    PTR_OP, INC_OP, DEC_OP, LEFT_OP, RIGHT_OP, LE_OP, GE_OP, EQ_OP, NE_OP,
    AND_OP, OR_OP, MUL_ASSIGN, DIV_ASSIGN, MOD_ASSIGN, ADD_ASSIGN, SUB_ASSIGN,
    LEFT_ASSIGN, RIGHT_ASSIGN, AND_ASSIGN, XOR_ASSIGN, OR_ASSIGN,

    SEMICOLON, OPEN_BRACE, CLOSE_BRACE, COMMA, COLON, EQ, OPEN_PAREN,
    CLOSE_PAREN, OPEN_BRACKET, CLOSE_BRACKET, DOT, AND, BANG, TILDE, MINUS,
    PLUS, STAR, SLASH, PERCENT, LESS, GREATER, HAT, PIPE, QUESTION,

    // keywords
    SIZEOF, TYPEDEF, EXTERN, STATIC, AUTO, REGISTER, CHAR, SHORT, INT, LONG,
    SIGNED, UNSIGNED, FLOAT, DOUBLE, CONST, VOLATILE, VOID, STRUCT, UNION,
    ENUM, CASE, DEFAULT, IF, ELSE, SWITCH, WHILE, DO, FOR, GOTO, CONTINUE,
    BREAK, RETURN,

    // ELLIPSIS,
    // clang-format on
  } kind;

  char *data;
  char *end;
};

struct ParseState {
  // [start, end[ contains the current data buffer.
  char *start;
  char *end;

  // Pointer in [start, end[ where we're currently parsing.
  char *current;

  // Currently parsed token.
  struct Token curToken;
};

// Represents an expression in the AST.
struct ExprAST {
  enum {
    INT_EXPR,      // 1
    STR_EXPR,      // "foo"
    VARIABLE_EXPR, // foo
    ARRAY_EXPR,    // {a, b, c}

    CALL_EXPR,   // lhs()
    INDEX_EXPR,  // lhs[rhs]
    MEMBER_EXPR, // primary.identifier or primary->identifier based on op
    UNARY_EXPR,  // lhs++, lhs-- or --rhs ++rhs based on op
    SIZEOF_EXPR, // sizeof rhs

    CONDITIONAL_EXPR, // cond ? lhs : rhs

    ARG_LIST, // lhs, rhs

    BINARY_EXPR, // a + b, ...
  } kind;

  // primary_expr
  // \{
  // int
  int value;
  // \}

  // binary
  struct Token op;
  struct ExprAST *lhs;
  struct ExprAST *rhs;

  struct Token identifier;

  struct ExprAST *cond;

  struct Type *type;
};

struct Type {
  enum {
    VOID_TYPE,
    CHAR_TYPE,
    INT_TYPE,
    STRUCT_TYPE,
    ENUM_TYPE,
    POINTER_TYPE,
    ARRAY_TYPE,
    FUNC_TYPE,
  } kind;

  // For tagged structs / enums.
  struct Token tag;

  struct Type *result;

  // For pointers / arrays, the contained type
  // For functions, linked list of arg types.
  struct Type *arg;
  struct Type *argNext;

  int isConst;
};

struct DeclAST {
  enum { VAR_DECL, STRUCT_DECL, ENUM_DECL, FUNC_DECL } kind;

  struct Type *type;

  struct Token name;

  // For var decl.
  struct ExprAST *init;

  // For struct decls, linked list of fields.
  // For funcs, linked list of args
  struct DeclAST *fields;
  struct DeclAST *fieldNext;

  // For function defs
  struct StmtAST *body;
};

struct StmtAST {
  enum {
    DECL_STMT,
    COMPOUND_STMT,
    EXPR_STMT,

    FOR_STMT,    // for(init, cond, expr) stmt
    IF_STMT,     // if (expr) init else stmt
    WHILE_STMT,  // while(expr) stmt
    SWITCH_STMT, // switch(expr) stmt

    RETURN_STMT,
    CASE_STMT,
    BREAK_STMT,
    DEFAULT_STMT,
  } kind;

  struct DeclAST *decl;

  // For for
  struct StmtAST *init;
  struct StmtAST *cond;

  // For expr stmts
  struct ExprAST *expr;

  // For compound stmts
  struct StmtAST *stmt;

  // To form linked list of compound stmts;
  struct StmtAST *nextStmt;
};

/// debug only:
/// \{

const char *token_str[] = {
    "TOK_EOF",  "IDENTIFIER", "CONSTANT", "STRING_LITERAL",
    "->",       "++",         "--",       "<<",
    ">>",       "<=",         ">=",       "==",
    "!=",       "&&",         "||",       "*=",
    "/=",       "%=",         "+=",       "-=",
    "<<=",      ">>=",        "&=",       "^=",
    "|=",       ";",          "{",        "}",
    ",",        ":",          "=",        "(",
    ")",        "[",          "]",        ".",
    "&",        "!",          "~",        "-",
    "+",        "*",          "/",        "%",
    "<",        ">",          "^",        "|",
    "?",        "SIZEOF",     "TYPEDEF",  "EXTERN",
    "STATIC",   "AUTO",       "REGISTER", "CHAR",
    "SHORT",    "INT",        "LONG",     "SIGNED",
    "UNSIGNED", "FLOAT",      "DOUBLE",   "CONST",
    "VOLATILE", "VOID",       "STRUCT",   "UNION",
    "ENUM",     "CASE",       "DEFAULT",  "IF",
    "ELSE",     "SWITCH",     "WHILE",    "DO",
    "FOR",      "GOTO",       "CONTINUE", "BREAK",
    "RETURN",
};

void printStr(char *start, char *end) {
  for (char *c = start; c != end; c++) {
    putchar(*c);
  }
}

void printToken(struct Token token) {
  printf("%s", token_str[token.kind]);
  printf("(");
  printStr(token.data, token.end);
  printf(") ");
}

void failParseArg(struct ParseState *state, const char *msg, const char *arg) {
  int line = 1;
  int column = 0;
  for (const char *c = state->start; c < state->current && c < state->end;
       c++) {
    if (*c == '\n') {
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

void failParse(struct ParseState *state, const char *msg) {
  failParseArg(state, msg, "");
}

void printType(struct Type *type) {
  if (type->isConst) {
    printf("const ");
  }
  switch (type->kind) {
  case INT_TYPE:
    printf("int ");
    break;
  case VOID_TYPE:
    printf("void ");
    break;
  case CHAR_TYPE:
    printf("char ");
    break;
  case POINTER_TYPE:
    printType(type->arg);
    printf("* ");
    break;
  case ARRAY_TYPE:
    printType(type->arg);
    printf("[] ");
    break;
  case STRUCT_TYPE:
    printf("struct ");
    printStr(type->tag.data, type->tag.end);
    printf(" ");
    break;
  case ENUM_TYPE:
    printf("enum ");
    if (type->tag.data) {
      printStr(type->tag.data, type->tag.end);
    }
    printf(" ");
    break;
  case FUNC_TYPE:
    printf("(");
    for (struct Type *arg = type->arg; arg != NULL; arg = arg->argNext) {
      printType(arg);
      if (arg->argNext != NULL) {
        printf(",");
      }
    }
    printf(") -> ");
    printType(type->result);
    break;
  }
}

void printExpr(struct ExprAST *expr) {
  if (expr == NULL) {
    puts("ERROR! null expr");
    return;
  }

  switch (expr->kind) {
  case VARIABLE_EXPR:
    printToken(expr->identifier);
    break;
  case INT_EXPR:
    printf("INT(%d)", expr->value);
    break;
  case STR_EXPR:
    printf("STR(");
    printStr(expr->identifier.data, expr->identifier.end);
    printf(")");
    break;
  case BINARY_EXPR:
    printf("%s(", token_str[expr->op.kind]);
    printExpr(expr->lhs);
    printf(" ");
    printExpr(expr->rhs);
    printf(")");
    break;
  case INDEX_EXPR:
    printf("INDEX(");
    printExpr(expr->lhs);
    printf(" ");
    printExpr(expr->rhs);
    printf(")");
    break;
  case CALL_EXPR:
    printf("CALL(");
    printExpr(expr->lhs);
    for (struct ExprAST *cur = expr->rhs; cur != NULL; cur = cur->rhs) {
      printf(" ");
      printExpr(cur->lhs);
    }
    printf(")");
    break;
  case MEMBER_EXPR:
    printf("MEMBER(");
    printExpr(expr->lhs);
    printf(" %s ", token_str[expr->op.kind]);
    printStr(expr->identifier.data, expr->identifier.end);
    printf(")");
    break;
  case UNARY_EXPR:
    printf("UNARY(");
    if (expr->lhs) {
      printExpr(expr->lhs);
    }
    printf("%s", token_str[expr->op.kind]);
    if (expr->rhs) {
      printExpr(expr->rhs);
    }
    printf(")");
    break;
  case SIZEOF_EXPR:
    printf("SIZEOF(");
    if (expr->type) {
      printType(expr->type);
    } else {
      printExpr(expr->rhs);
    }
    printf(")");
    break;
  case CONDITIONAL_EXPR:
    printf("COND(");
    printExpr(expr->cond);
    printf(" ? ");
    printExpr(expr->lhs);
    printf(" : ");
    printExpr(expr->rhs);
    printf(")");
    break;
  case ARRAY_EXPR:
    printf("ARRAY(");
    for (; expr != NULL; expr = expr->rhs) {
      printExpr(expr->lhs);
      printf(", ");
    }
    printf(")");
    break;
  case ARG_LIST:
    break;
  }
}

void printDecl(struct DeclAST *decl);

void printStmt(struct StmtAST *stmt) {
  switch (stmt->kind) {
  case DECL_STMT:
    printDecl(stmt->decl);
    break;
  case COMPOUND_STMT:
    printf("{\n");
    for (struct StmtAST *cur = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      printStmt(cur);
    }
    printf("}\n");
    break;
  case EXPR_STMT:
    printExpr(stmt->expr);
    printf(";\n");
    break;
  case FOR_STMT:
    printf("for(\n");
    printf("  ");
    printStmt(stmt->init);
    printf("  ");
    printStmt(stmt->cond);
    printf("  ");
    printExpr(stmt->expr);
    printf("\n):");
    printStmt(stmt->stmt);
    break;
  case IF_STMT:
    printf("if(");
    printExpr(stmt->expr);
    printf(")\n");
    printStmt(stmt->init);
    if (stmt->stmt) {
      printf("else\n");
      printStmt(stmt->stmt);
    }
    break;
  case RETURN_STMT:
    printf("return ");
    if (stmt->expr) {
      printExpr(stmt->expr);
    }
    printf(";\n");
    break;
  case SWITCH_STMT:
    printf("switch (");
    printExpr(stmt->expr);
    printf(")\n");
    printStmt(stmt->stmt);
    break;
  case DEFAULT_STMT:
    printf("default:\n");
    break;
  case BREAK_STMT:
    printf("break;\n");
    break;
  case CASE_STMT:
    printf("case ");
    printExpr(stmt->expr);
    printf(":\n");
    break;
  case WHILE_STMT:
    printf("while (");
    printExpr(stmt->expr);
    printf(")\n");
    printStmt(stmt->stmt);
    break;
  }
}

void printDecl(struct DeclAST *decl) {
  switch (decl->kind) {
  case STRUCT_DECL:
    printType(decl->type);
    printf("{\n");
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->fieldNext) {
      printf("  ");
      printDecl(field);
    }
    printf("}");
    break;
  case ENUM_DECL:
    printType(decl->type);
    printf("{\n");
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->fieldNext) {
      printf("  ");
      printDecl(field);
    }
    printf("} ");
    printToken(decl->name);
    break;
  case VAR_DECL:
    printType(decl->type);
    printToken(decl->name);

    if (decl->init != NULL) {
      printf(" = ");
      printExpr(decl->init);
    }
    break;
  case FUNC_DECL:
    printType(decl->type);
    printToken(decl->name);
    printf(":\n");
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->fieldNext) {
      printf("  ");
      printDecl(field);
    }

    if (decl->body != NULL) {
      printStmt(decl->body);
    }
    break;
  }
  printf("\n");
}
/// end debug only
/// \}

const char *keywords[] = {
    "sizeof",  "typedef",  "extern", "static", "auto",     "register", "char",
    "short",   "int",      "long",   "signed", "unsigned", "float",    "double",
    "const",   "volatile", "void",   "struct", "union",    "enum",     "case",
    "default", "if",       "else",   "switch", "while",    "do",       "for",
    "goto",    "continue", "break",  "return",
};

// Returns the current character and advances the current pointer.
int nextChar(struct ParseState *state) {
  if (state->current == state->end) {
    return EOF;
  }

  int result = *state->current;
  state->current++;
  return result;
}

// Returns the current character without advancing
int peekChar(struct ParseState *state) {
  if (state->current == state->end) {
    return EOF;
  }
  return *state->current;
}

/// True if the current character is an EOL character
int iseol(int c) { return c == '\n' || c == '\r'; }

struct Token getToken(struct ParseState *state) {
  char *tokenStart = state->current;
  char lastChar = nextChar(state);
  struct Token token;

  // Eat whitespace
  while (isspace(lastChar)) {
    tokenStart = state->current;
    lastChar = nextChar(state);
  }

  if (lastChar == EOF) {
    token.kind = TOK_EOF;
    return token;
  }

  // identifier [a-zA-Z][a-zA-Z0-9]*
  if (isalpha(lastChar) || lastChar == '_') {
    while (isalnum(peekChar(state)) || peekChar(state) == '_') {
      nextChar(state);
    }

    token.data = tokenStart;
    token.end = state->current; // one past the end!

    for (int i = 0; i < sizeof(keywords) / sizeof(keywords[0]); i++) {
      if (memcmp(token.data, keywords[i], strlen(keywords[i])) == 0) {
        token.kind = SIZEOF + i;
        return token;
      }
    }

    token.kind = IDENTIFIER;
    return token;
  }

  if (lastChar == '\'') {
    while (peekChar(state) != '\'') {
      int next = nextChar(state);
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
      int next = nextChar(state);
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

  if (isdigit(lastChar) || (lastChar == '-' && isdigit(peekChar(state)))) {
    while (isdigit(peekChar(state)) || peekChar(state) == '.') {
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
  if (lastChar == '=' && peekChar(state) == '=') {
    nextChar(state);
    token.kind = EQ_OP;
  } else if (lastChar == '-' && peekChar(state) == '>') {
    nextChar(state);
    token.kind = PTR_OP;
  } else if (lastChar == '|' && peekChar(state) == '|') {
    nextChar(state);
    token.kind = OR_OP;
  } else if (lastChar == '&' && peekChar(state) == '&') {
    nextChar(state);
    token.kind = AND_OP;
  } else if (lastChar == '!' && peekChar(state) == '=') {
    nextChar(state);
    token.kind = NE_OP;
  } else if (lastChar == '+' && peekChar(state) == '+') {
    nextChar(state);
    token.kind = INC_OP;
  } else if (lastChar == '-' && peekChar(state) == '-') {
    nextChar(state);
    token.kind = DEC_OP;
  } else if (lastChar == '+' && peekChar(state) == '=') {
    nextChar(state);
    token.kind = ADD_ASSIGN;
  } else {
    switch (lastChar) {
      // clang-format off
      case ';': token.kind = SEMICOLON; break;
      case '{': token.kind = OPEN_BRACE; break;
      case '}': token.kind = CLOSE_BRACE; break;
      case ':': token.kind = COLON; break;
      case '=': token.kind = EQ; break;
      case '(': token.kind = OPEN_PAREN; break;
      case ')': token.kind = CLOSE_PAREN; break;
      case '[': token.kind = OPEN_BRACKET; break;
      case ']': token.kind = CLOSE_BRACKET; break;
      case '.': token.kind = DOT; break;
      case '&': token.kind = AND; break;
      case '!': token.kind = BANG; break;
      case '~': token.kind = TILDE; break;
      case '-': token.kind = MINUS; break;
      case '+': token.kind = PLUS; break;
      case '*': token.kind = STAR; break;
      case '/': token.kind = SLASH; break;
      case '%': token.kind = PERCENT; break;
      case '<': token.kind = LESS; break;
      case '>': token.kind = GREATER; break;
      case '^': token.kind = HAT; break;
      case '|': token.kind = PIPE; break;
      case '?': token.kind = QUESTION; break;
      case ',': token.kind = COMMA; break;
      default:
        printf("Unknown token! %c", lastChar);
        failParse(state, "Unknown token");
      // clang-format on
    }
  }

  token.data = tokenStart;
  token.end = state->current;

  return token;
}

int match(struct ParseState *state, int tok) {
  return state->curToken.kind == tok;
}

void expect(struct ParseState *state, int tok) {
  if (!match(state, tok)) {
    failParseArg(state, "Expected: ", token_str[tok]);
  }
}

struct Token getNextToken(struct ParseState *state) {
  struct Token result = state->curToken;
  state->curToken = getToken(state);
  return result;
}

struct ExprAST *newExpr(int kind) {
  struct ExprAST *result = malloc(sizeof(struct ExprAST));
  result->kind = kind;
  return result;
}

struct ExprAST *parseNumber(struct ParseState *state) {
  struct ExprAST *result = newExpr(INT_EXPR);

  char *start = state->curToken.data;
  if (*start == '\'') {
    // TODO: handle escapes.
    result->value = start[1];
  } else {
    char *endp = state->curToken.end;
    int num = strtol(start, &endp, 10);
    result->value = num;
  }

  getNextToken(state);
  return result;
}

struct ExprAST *parseString(struct ParseState *state) {
  struct ExprAST *result = newExpr(STR_EXPR);
  // TODO: handle escapes.
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

int isDecl(struct Token tok) {
  // We don't support typedef, so this is easy
  switch (tok.kind) {
  case CONST:
  case STRUCT:
  case ENUM:
  case INT:
  case CHAR:
  case VOID:
    return 1;
  default:
    return 0;
  }
}

struct DeclAST *newDecl() {
  struct DeclAST *decl = malloc(sizeof(struct DeclAST));
  return decl;
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
      expr->type = dummy->type;
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
  // TODO: no need for casts?
  return parseUnary(state);
}

// TODO: dedup code?
// struct ExprAST *parseFactor(struct ParseState *state) {
//   struct ExprAST *expr = parseCast(state);
//   if (expr == NULL) {
//     return expr;
//   }
//   while (match(state, STAR) || match(state, SLASH) || match(state, PERCENT))
//   {
//     struct Token op = getNextToken(state);
//     struct ExprAST *rhs = parsePrimary(state);
//     if (rhs == NULL) {
//       return expr;
//     }
//
//     struct ExprAST *new = newExpr(BINARY_EXPR);
//     new->lhs = expr;
//     new->op = op;
//     new->rhs = rhs;
//
//     expr = new;
//   }
//   return expr;
// }
//

int getPrecedence(struct Token tok) {
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

struct ExprAST *parseBinOpRhs(struct ParseState *state, int prec,
                              struct ExprAST *lhs) {
  while (1) {
    int curPred = getPrecedence(state->curToken);
    if (curPred < prec) {
      return lhs;
    }

    struct Token op = state->curToken;
    getNextToken(state);

    struct ExprAST *rhs = parseCast(state);

    int nextPred = getPrecedence(state->curToken);
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
  if (!match(state, EQ) && !match(state, MUL_ASSIGN) &&
      !match(state, DIV_ASSIGN) && !match(state, MOD_ASSIGN) &&
      !match(state, ADD_ASSIGN) && !match(state, SUB_ASSIGN) &&
      !match(state, LEFT_ASSIGN) && !match(state, RIGHT_ASSIGN) &&
      !match(state, AND_ASSIGN) && !match(state, XOR_ASSIGN) &&
      !match(state, OR_ASSIGN)) {
    return lhs;
  }
  struct Token op = state->curToken;
  getNextToken(state);

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

struct Type *newType(int kind) {
  struct Type *type = malloc(sizeof(struct Type));
  type->kind = kind;
  return type;
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
    fields->fieldNext = parseNoInitDecl(state);
    expect(state, SEMICOLON);
    getNextToken(state); // eat ;
    fields = fields->fieldNext;
  }
  getNextToken(state); // eat }

  fields->fieldNext = NULL;
  decl->fields = decl->fieldNext;
  decl->fieldNext = NULL;
}

void parseEnum(struct ParseState *state, struct DeclAST *decl) {
  getNextToken(state);
  decl->type = newType(ENUM_TYPE);

  // not supported: enum tag.
  expect(state, OPEN_BRACE);
  getNextToken(state);

  decl->kind = ENUM_DECL;

  // parse constants
  struct DeclAST *fields = decl;
  while (!match(state, CLOSE_BRACE)) {
    expect(state, IDENTIFIER);

    struct DeclAST *field = newDecl();
    field->kind = VAR_DECL;
    field->type = newType(INT_TYPE);

    field->name = getNextToken(state);

    fields->fieldNext = field;
    fields = field;

    if (match(state, CLOSE_BRACE)) {
      break;
    }

    expect(state, COMMA);
    getNextToken(state);
  }
  getNextToken(state); // eat }

  fields->fieldNext = NULL;
  decl->fields = decl->fieldNext;
  decl->fieldNext = NULL;
}

void parseDeclSpecifier(struct ParseState *state, struct DeclAST *decl) {
  int isConst = 0;
  if (match(state, CONST)) {
    getNextToken(state);
    isConst = 1;
  }

  // struct type ref or decl.
  if (match(state, STRUCT)) {
    parseStruct(state, decl);
  } else if (match(state, ENUM)) {
    parseEnum(state, decl);
  } else if (match(state, INT)) {
    getNextToken(state);
    decl->type = newType(INT_TYPE);
  } else if (match(state, CHAR)) {
    getNextToken(state);
    decl->type = newType(CHAR_TYPE);
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
    struct DeclAST *param = parseNoInitDecl(state);
    curDecl->fieldNext = param;
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

  decl->fields = decl->fieldNext;
  decl->fieldNext = NULL;
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

struct StmtAST *newStmt(int kind) {
  struct StmtAST *stmt = malloc(sizeof(struct StmtAST));
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

struct StmtAST *parseSwitchOrWhileStmt(struct ParseState *state, int kind) {
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

int main(int argc, char **argv) {
  if (argc != 2) {
    puts("Usage: compile file.c");
    return -1;
  }

  int fd = open(argv[1], O_RDONLY);
  if (fd == -1) {
    return -1;
  }

  int size = lseek(fd, 0, SEEK_END);
  if (size == -1) {
    return -1;
  }

  if (lseek(fd, 0, SEEK_SET) == -1) {
    return -1;
  }

  char *fileMem = malloc(size);

  int off = 0;
  while (off != size) {
    int r = read(fd, fileMem + off, size - off);
    if (r == -1) {
      return -1;
    }
    off += r;
  }

  struct ParseState state;
  state.current = state.start = fileMem;
  state.end = fileMem + size;

  getNextToken(&state);

  while (state.curToken.kind != TOK_EOF) {
    // printToken(state.curToken);
    // getNextToken(&state);

    // struct ExprAST *expr = parseExpression(&state);
    // printExpr(expr);

    // struct StmtAST *stmt = parseStmt(&state);
    // printStmt(stmt);

    struct DeclAST *decl = parseDeclarationOrFunction(&state);
    printDecl(decl);
  }

  // No cleanup needed
  return 0;
}
