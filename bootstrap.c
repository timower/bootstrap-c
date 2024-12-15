#include <ctype.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if 0
int putchar(int __c);
int puts(const char *s);
int printf(const char *format, ...);
int sprintf (char * s, const char * format, ...);
void exit(int status);
int strcmp (const char *s1, const char *s2);
int open (const char *file, int oflags, ...);

// TODO: size_t
// \{
int memcmp(const void *s1, const void *s2, int n);
int strlen(const char *s);
void *malloc(int size);
void *calloc(int count, int size);

// long
int strtol(const char *ptr, char **end, int base);
int lseek (int fd, int offset, int whence);
int read (int fd, void *buf, int nbytes);
// \}
#endif

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

    ELLIPSIS,

    SEMICOLON, OPEN_BRACE, CLOSE_BRACE, COMMA, COLON, EQ, OPEN_PAREN,
    CLOSE_PAREN, OPEN_BRACKET, CLOSE_BRACKET, DOT, AND, BANG, TILDE, MINUS,
    PLUS, STAR, SLASH, PERCENT, LESS, GREATER, HAT, PIPE, QUESTION,

    // keywords
    SIZEOF, TYPEDEF, EXTERN, STATIC, AUTO, REGISTER, CHAR, SHORT, INT, LONG,
    SIGNED, UNSIGNED, FLOAT, DOUBLE, CONST, VOLATILE, VOID, STRUCT, UNION,
    ENUM, CASE, DEFAULT, IF, ELSE, SWITCH, WHILE, DO, FOR, GOTO, CONTINUE,
    BREAK, RETURN,

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
    INT_EXPR,      // value
    STR_EXPR,      // "identifier"
    VARIABLE_EXPR, // identifier

    ARRAY_EXPR, // {a, b, c}

    CALL_EXPR,  // lhs(rhs->lhs, rhs->rhs->lhs, ..)
    INDEX_EXPR, // lhs[rhs]

    MEMBER_EXPR, // lhs.identifier or lhs->identifier based on op, value is
                 // field index after sema

    UNARY_EXPR,  // lhs++, lhs-- or --rhs ++rhs based on op
    SIZEOF_EXPR, // sizeof rhs

    CONDITIONAL_EXPR, // cond ? lhs : rhs

    ARG_LIST, // lhs, rhs

    BINARY_EXPR, // a + b, ...

    CAST_EXPR, // (type)lhs
  } kind;

  struct Type *type;

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

  struct Type *sizeofArg;
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

  // For array types
  int size;

  // For funcs
  int isVarargs;

  // TODO: remove
  int isDecay;
};

struct DeclAST {
  enum {
    VAR_DECL,
    STRUCT_DECL,
    ENUM_DECL,
    FUNC_DECL,
  } kind;

  struct Type *type;

  struct Token name;

  // For var decl.
  struct ExprAST *init;

  // For struct decls, linked list of fields.
  // For funcs, linked list of args
  struct DeclAST *fields;
  struct DeclAST *next;

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

struct VarType {
  struct Token name;

  struct Type *type;
  int enumValue;

  struct VarType *next;
};

// Struct defs only currently
struct TypeDef {
  struct DeclAST *decl;

  struct TypeDef *next;
};

struct SemaState {
  struct SemaState *parent;

  // Return type of the current function.
  struct Type *result;

  struct VarType *locals;
  struct TypeDef *types;
};

struct EmitState {
  int tmpCounter;
  struct LocalVar *vars;

  char *curBreakLabel;
  char *defaultLabel;
  struct Case *cases;

  struct EmitState *parent;
};

// LLVM IR Value
struct Value {
  const char *type;

  // reg name or just the value
  const char *val;
};

struct LocalVar {
  struct Token name;
  struct Value value;

  struct LocalVar *next;
};

struct Case {
  struct Value val;
  int n;

  struct Case *next;
};

/// debug only:
/// \{

const char *token_str[] = {
    "TOK_EOF", "IDENTIFIER", "CONSTANT", "STRING_LITERAL",
    "->",      "++",         "--",       "<<",
    ">>",      "<=",         ">=",       "==",
    "!=",      "&&",         "||",       "*=",
    "/=",      "%=",         "+=",       "-=",
    "<<=",     ">>=",        "&=",       "^=",
    "|=",      "...",        ";",        "{",
    "}",       ",",          ":",        "=",
    "(",       ")",          "[",        "]",
    ".",       "&",          "!",        "~",
    "-",       "+",          "*",        "/",
    "%",       "<",          ">",        "^",
    "|",       "?",          "SIZEOF",   "TYPEDEF",
    "EXTERN",  "STATIC",     "AUTO",     "REGISTER",
    "CHAR",    "SHORT",      "INT",      "LONG",
    "SIGNED",  "UNSIGNED",   "FLOAT",    "DOUBLE",
    "CONST",   "VOLATILE",   "VOID",     "STRUCT",
    "UNION",   "ENUM",       "CASE",     "DEFAULT",
    "IF",      "ELSE",       "SWITCH",   "WHILE",
    "DO",      "FOR",        "GOTO",     "CONTINUE",
    "BREAK",   "RETURN",
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
    printf("[%d] ", type->size);
    break;
  case STRUCT_TYPE:
    printf("struct ");
    printStr(type->tag.data, type->tag.end);
    printf(" ");
    break;
  case ENUM_TYPE:
    printf("enum ");
    if (type->tag.data != NULL) {
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
    if (type->isVarargs) {
      printf("  ...");
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

  if (expr->type != NULL) {
    printType(expr->type);
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
    if (expr->lhs != NULL) {
      printExpr(expr->lhs);
    }
    printf("%s", token_str[expr->op.kind]);
    if (expr->rhs != NULL) {
      printExpr(expr->rhs);
    }
    printf(")");
    break;
  case SIZEOF_EXPR:
    printf("SIZEOF(");
    if (expr->sizeofArg != NULL) {
      printType(expr->sizeofArg);
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
  case CAST_EXPR:
    printf("CAST(");
    printExpr(expr->lhs);
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
         field = field->next) {
      printf("  ");
      printDecl(field);
    }
    printf("}");
    break;
  case ENUM_DECL:
    printType(decl->type);
    printf("{\n");
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->next) {
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
         field = field->next) {
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

// utils
int tokCmp(struct Token one, struct Token two) {
  if (one.kind != two.kind) {
    return 0;
  }

  int len1 = one.end - one.data;
  int len2 = two.end - two.data;
  if (len1 != len2) {
    return 0;
  }

  return memcmp(one.data, two.data, len1) == 0;
}

int tokCmpStr(struct Token one, const char *str) {
  int len1 = one.end - one.data;
  int len2 = strlen(str);
  if (len1 != len2) {
    return 0;
  }

  return memcmp(one.data, str, len1) == 0;
}

// 1. parse
const char *tokens[] = {
    "->",      "++",       "--",     "<<",     ">>",       "<=",       ">=",
    "==",      "!=",       "&&",     "||",     "*=",       "/=",       "%=",
    "+=",      "-=",       "<<=",    ">>=",    "&=",       "^=",       "|=",

    "...",     ";",        "{",      "}",      ",",        ":",        "=",
    "(",       ")",        "[",      "]",      ".",        "&",        "!",
    "~",       "-",        "+",      "*",      "/",        "%",        "<",
    ">",       "^",        "|",      "?",

    "sizeof",  "typedef",  "extern", "static", "auto",     "register", "char",
    "short",   "int",      "long",   "signed", "unsigned", "float",    "double",
    "const",   "volatile", "void",   "struct", "union",    "enum",     "case",
    "default", "if",       "else",   "switch", "while",    "do",       "for",
    "goto",    "continue", "break",  "return",
};

// Returns the current character and advances the current pointer.
int nextChar(struct ParseState *state) {
  if (state->current >= state->end) {
    return -1;
  }

  int result = *state->current;
  state->current++;
  return result;
}

// Returns the current character without advancing
int peekChar(struct ParseState *state) {
  if (state->current >= state->end) {
    return -1;
  }
  return *state->current;
}

/// True if the current character is an EOL character
int iseol(int c) { return c == '\n' || c == '\r'; }
int is_space(int c) { return iseol(c) || c == ' ' || c == '\t'; }
int is_alpha(int c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }
int is_digit(int c) { return c >= '0' && c <= '9'; }
int is_alnum(int c) { return is_digit(c) || is_alpha(c); }

struct Token getToken(struct ParseState *state) {
  char *tokenStart = state->current;
  char lastChar = nextChar(state);
  struct Token token;

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
    for (int i = 0; i < sizeof(tokens) / sizeof(tokens[0]); i++) {
      if (tokCmpStr(token, tokens[i])) {
        token.kind = PTR_OP + i;
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
  for (int i = 0; i < sizeof(tokens) / sizeof(tokens[0]); i++) {
    int len = strlen(tokens[i]);
    int remaining = state->end - tokenStart;
    if (len < remaining && memcmp(tokenStart, tokens[i], len) == 0) {
      token.kind = PTR_OP + i;
      token.data = tokenStart;

      state->current = tokenStart + len;
      token.end = state->current;

      return token;
    }
  }

  printf("Unknown token! %c", lastChar);
  failParse(state, "Unknown token");
  token.kind = TOK_EOF;
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
  struct ExprAST *result = calloc(1, sizeof(struct ExprAST));
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
  struct DeclAST *decl = calloc(1, sizeof(struct DeclAST));
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

int isAssign(struct Token tok) {
  switch (tok.kind) {
  case EQ:
  case MUL_ASSIGN:
  case DIV_ASSIGN:
  case MOD_ASSIGN:
  case ADD_ASSIGN:
  case SUB_ASSIGN:
  case LEFT_ASSIGN:
  case RIGHT_ASSIGN:
  case AND_ASSIGN:
  case XOR_ASSIGN:
  case OR_ASSIGN:
    return 1;
  default:
    return 0;
  }
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

struct Type *newType(int kind) {
  struct Type *type = calloc(1, sizeof(struct Type));
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

struct StmtAST *newStmt(int kind) {
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

struct DeclAST *parseTopLevel(struct ParseState *state) {
  getNextToken(state); // Prep token parser

  struct DeclAST *lastDecl = NULL;
  struct DeclAST *firstDecl = NULL;
  while (state->curToken.kind != TOK_EOF) {
    struct DeclAST *decl = parseDeclarationOrFunction(state);

    if (lastDecl == NULL) {
      firstDecl = decl;
    } else {
      lastDecl->next = decl;
    }
    lastDecl = decl;
  }
  return firstDecl;
}

// 2. sema
void failSema(const char *msg) {
  puts(msg);
  exit(1);
}

int typeEq(struct Type *one, struct Type *two) {
  if (one->kind != two->kind) {
    return 0;
  }

  switch (one->kind) {
  case INT_TYPE:
  case VOID_TYPE:
  case CHAR_TYPE:
    // TODO type safe enums
    // All enums are typed as 'int'
  case ENUM_TYPE:
    break;

  case ARRAY_TYPE:
    if (one->size != 0 && two->size != 0 && one->size != two->size) {
      return 0;
    }
  case POINTER_TYPE:
    return typeEq(one->arg, two->arg);

  case STRUCT_TYPE:
    return tokCmp(one->tag, two->tag);

  case FUNC_TYPE:
    failSema("TODO: type eq struct, enum, func");
    break;
  }
  return 1;
}

// TODO: Remove implicit casts after adding 'as'
struct ExprAST *doConvert(struct ExprAST *expr, struct Type *to) {
  struct Type *from = expr->type;

  if (typeEq(from, to)) {
    return expr;
  }

  // Int and enums are equivalent currently.
  if ((from->kind == ENUM_TYPE && to->kind == INT_TYPE) ||
      (from->kind == INT_TYPE && to->kind == ENUM_TYPE)) {
    return expr;
  }

  // int and char can be converted.
  if ((from->kind == INT_TYPE && to->kind == CHAR_TYPE) ||
      (from->kind == CHAR_TYPE && to->kind == INT_TYPE)) {
    struct ExprAST *res = newExpr(CAST_EXPR);
    res->lhs = expr;
    res->type = to;
    return res;
  }

  // void * can be converted from and to any other pointer..
  if (from->kind == POINTER_TYPE && to->kind == POINTER_TYPE &&
      (from->arg->kind == VOID_TYPE || to->arg->kind == VOID_TYPE)) {
    return expr;
  }

  return NULL;
}

struct Type *getCommonType(struct Type *a, struct Type *b) {
  if (typeEq(a, b)) {
    return a;
  }

  // int, enum -> int
  if (a->kind == INT_TYPE && b->kind == ENUM_TYPE) {
    return a;
  }
  if (b->kind == INT_TYPE && a->kind == ENUM_TYPE) {
    return b;
  }

  // int, char -> int
  if (a->kind == INT_TYPE && b->kind == CHAR_TYPE) {
    return a;
  }
  if (b->kind == INT_TYPE && a->kind == CHAR_TYPE) {
    return b;
  }

  if (a->kind == POINTER_TYPE && b->kind == POINTER_TYPE) {
    if (a->arg->kind == VOID_TYPE) {
      return a;
    }
    if (b->arg->kind == VOID_TYPE) {
      return b;
    }
  }
  return NULL;
}

// TODO: remove implicit pointer to int conversion for if(ptr) and if (!ptr)
void checkBool(struct ExprAST *expr) {
  if (expr->type->kind != INT_TYPE && expr->type->kind != POINTER_TYPE) {
    printExpr(expr);
    failSema(": Expected bool!");
  }
}

struct TypeDef *newTypeDef(struct DeclAST *decl) {
  struct TypeDef *res = calloc(1, sizeof(struct TypeDef));
  res->decl = decl;
  return res;
}

struct DeclAST *findType(struct TypeDef *types, struct Token tag) {
  for (; types != NULL; types = types->next) {
    if (tokCmp(tag, types->decl->type->tag)) {
      return types->decl;
    }
  }
  return NULL;
}

struct DeclAST *lookupType(struct SemaState *state, struct Token tag) {
  for (; state != NULL; state = state->parent) {
    struct DeclAST *type = findType(state->types, tag);
    if (type != NULL) {
      return type;
    }
  }

  printToken(tag);
  failSema("Unknown type");
  return NULL;
}

struct VarType *newVarType(struct Token name, struct Type *type) {
  struct VarType *res = calloc(1, sizeof(struct VarType));
  res->name = name;
  res->type = type;
  return res;
}

struct VarType *findLocal(struct VarType *local, struct Token name) {
  for (; local != NULL; local = local->next) {
    if (tokCmp(name, local->name)) {
      return local;
    }
  }
  return NULL;
}

struct VarType *lookupLocal(struct SemaState *state, struct Token name) {
  for (; state != NULL; state = state->parent) {
    struct VarType *local = findLocal(state->locals, name);
    if (local != NULL) {
      return local;
    }
  }

  printToken(name);
  failSema("Unknown local");
  return NULL;
}

struct DeclAST *findField(struct DeclAST *structDecl, struct Token name,
                          int *idxOut) {
  int idx = 0;
  for (struct DeclAST *field = structDecl->fields; field != NULL;
       field = field->next, idx++) {
    if (tokCmp(name, field->name)) {
      *idxOut = idx;
      return field;
    }
  }
  return NULL;
}

int getSize(struct SemaState *state, struct Type *type) {
  switch (type->kind) {
  case VOID_TYPE:
    return 0;
  case INT_TYPE:
  case ENUM_TYPE:
    return 4;
  case CHAR_TYPE:
    return 1;

  case POINTER_TYPE:
  case FUNC_TYPE:
    return 8;

    // TODO: padding
  case ARRAY_TYPE:
    return type->size * getSize(state, type->arg);
  case STRUCT_TYPE: {
    struct DeclAST *decl = lookupType(state, type->tag);
    int size = 0;
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->next) {
      size += getSize(state, field->type);
    }
    return size == 0 ? 1 : size;
  }
  default:
    failSema("Unknown type for size");
    return 0;
  }
}

void semaExprNoDecay(struct SemaState *state, struct ExprAST *expr);

void semaExpr(struct SemaState *state, struct ExprAST *expr) {
  semaExprNoDecay(state, expr);

  // Decay array to pointer
  if (expr->type->kind == ARRAY_TYPE) {
    struct Type *decayType = newType(POINTER_TYPE);
    decayType->arg = expr->type->arg;
    decayType->isDecay = 1;
    expr->type = decayType;
  }
}

void semaBinExpr(struct SemaState *state, struct ExprAST *expr) {
  semaExpr(state, expr->lhs);
  semaExpr(state, expr->rhs);

  struct Type *commonType = getCommonType(expr->lhs->type, expr->rhs->type);

  switch (expr->op.kind) {
  case COMMA:
    expr->type = expr->rhs->type;
    break;
    // comparision results in int.
  case LESS:
  case GREATER:
  case LE_OP:
  case GE_OP:
  case EQ_OP:
  case NE_OP:
    // TODO: this is wrong?
    if (commonType == NULL) {
      printExpr(expr);
      failSema(": Binary op on different types");
    }

    expr->lhs = doConvert(expr->lhs, commonType);
    expr->rhs = doConvert(expr->rhs, commonType);

    expr->type = newType(INT_TYPE);
    return;

  case MINUS:
    if (expr->lhs->type->kind == POINTER_TYPE &&
        expr->rhs->type->kind == POINTER_TYPE) {

      if (expr->lhs->type->arg->kind != CHAR_TYPE ||
          expr->rhs->type->arg->kind != CHAR_TYPE) {
        // TODO: emit (expr) / sizeof(type)
        failSema("Only char pointer subtract supported");
      }

      expr->type = newType(INT_TYPE);
      break;
    }
  case PLUS:
    if (expr->op.kind != MINUS && expr->lhs->type->kind == INT_TYPE &&
        expr->rhs->type->kind == POINTER_TYPE) {
      expr->type = expr->rhs->type;
      break;
    }
  case ADD_ASSIGN:
  case SUB_ASSIGN:
    if (expr->lhs->type->kind == POINTER_TYPE &&
        expr->rhs->type->kind == INT_TYPE) {
      expr->type = expr->lhs->type;
      break;
    }

  default:
    if (isAssign(expr->op)) {
      expr->rhs = doConvert(expr->rhs, expr->lhs->type);
      if (expr->rhs == NULL) {
        printExpr(expr);
        failSema(": Assign doesn't match");
      }
      expr->type = expr->lhs->type;
      break;
    }

    if (commonType == NULL) {
      printExpr(expr);
      failSema(": Binary op on different types");
    }
    expr->lhs = doConvert(expr->lhs, commonType);
    expr->rhs = doConvert(expr->rhs, commonType);

    if (expr->lhs == NULL || expr->rhs == NULL) {
      failSema("Wot");
    }

    expr->type = commonType;
  }
}

void semaExprNoDecay(struct SemaState *state, struct ExprAST *expr) {
  switch (expr->kind) {
  case ARG_LIST:
    failSema("TODO: sema all exprs");
    break;

  case MEMBER_EXPR:
    semaExpr(state, expr->lhs);
    struct DeclAST *structDecl = NULL;
    if (expr->op.kind == PTR_OP) {
      if (expr->lhs->type->kind != POINTER_TYPE ||
          expr->lhs->type->arg->kind != STRUCT_TYPE) {
        failSema("Expected pointer to struct type for -> expr");
      }
      structDecl = lookupType(state, expr->lhs->type->arg->tag);
    } else if (expr->op.kind == DOT) {
      if (expr->lhs->type->kind != STRUCT_TYPE) {
        failSema("Expected struct type for . expr");
      }
      structDecl = lookupType(state, expr->lhs->type->tag);
    } else {
      failSema("Unknown member op");
    }
    struct DeclAST *fieldDecl =
        findField(structDecl, expr->identifier, &expr->value);
    if (fieldDecl == NULL) {
      printToken(expr->identifier);
      failSema(" Cannot find field");
    }
    expr->type = fieldDecl->type;
    break;

  case CALL_EXPR:
    semaExpr(state, expr->lhs);
    // We don't support function pointers
    if (expr->lhs->type->kind != FUNC_TYPE) {
      failSema("Must call function type");
    }
    struct Type *curArgTy = expr->lhs->type->arg;
    struct ExprAST *cur = expr->rhs;
    for (; cur != NULL; cur = cur->rhs) {
      semaExpr(state, cur->lhs);

      if (curArgTy != NULL) {
        cur->lhs = doConvert(cur->lhs, curArgTy);
        if (cur->lhs == NULL) {
          failSema("Arg type mismatch");
        }
      }
      if (curArgTy != NULL) {
        curArgTy = curArgTy->argNext;
      }
    }
    int isValidVararg = expr->lhs->type->isVarargs && curArgTy == NULL;
    if (!isValidVararg && (curArgTy == NULL) != (cur == NULL)) {
      failSema("Function call arg length mismatch");
    }
    expr->type = expr->lhs->type->result;
    break;

  case CONDITIONAL_EXPR:
    semaExpr(state, expr->cond);
    checkBool(expr->cond);
    semaExpr(state, expr->lhs);
    semaExpr(state, expr->rhs);
    if (!typeEq(expr->lhs->type, expr->rhs->type)) {
      failSema("?: lhs and rhs should have same type");
    }
    expr->type = expr->lhs->type;
    break;

  case ARRAY_EXPR:
    expr->type = newType(ARRAY_TYPE);
    for (struct ExprAST *sub = expr; sub != NULL; sub = sub->rhs) {
      semaExpr(state, sub->lhs);

      if (expr->type->arg == NULL) {
        expr->type->arg = sub->lhs->type;
      } else if (!typeEq(expr->type->arg, sub->lhs->type)) {
        failSema("Init must have consistent type");
      }

      expr->type->size++;
    }
    break;

  case STR_EXPR:
    expr->type = newType(ARRAY_TYPE);
    expr->type->arg = newType(CHAR_TYPE);
    expr->type->size = expr->identifier.end - expr->identifier.data + 1;
    expr->type->isConst = 1;
    break;

  case VARIABLE_EXPR: {
    struct VarType *local = lookupLocal(state, expr->identifier);
    if (local->type == NULL) {
      // enum value, transform this expr to an int.
      expr->kind = INT_EXPR;
      expr->value = local->enumValue;
      expr->type = newType(INT_TYPE);
    } else {
      expr->type = local->type;
    }
  } break;

  case INT_EXPR:
    expr->type = newType(INT_TYPE);
    return;

  case BINARY_EXPR:
    semaBinExpr(state, expr);
    break;

  case INDEX_EXPR:
    semaExpr(state, expr->lhs);
    if (expr->lhs->type->kind != POINTER_TYPE &&
        expr->lhs->type->kind != ARRAY_TYPE) {
      failSema("Can't index non array or pointer");
    }
    semaExpr(state, expr->rhs);
    if (expr->rhs->type->kind != INT_TYPE &&
        expr->rhs->type->kind != ENUM_TYPE) {
      failSema("Can't index with non integer");
    }
    expr->type = expr->lhs->type->arg;
    break;

  case UNARY_EXPR:
    if (expr->op.kind == AND) {
      semaExprNoDecay(state, expr->rhs);
      expr->type = newType(POINTER_TYPE);
      expr->type->arg = expr->rhs->type;
      return;
    }

    if (expr->lhs) {
      semaExpr(state, expr->lhs);
      expr->type = expr->lhs->type;
    } else {
      semaExpr(state, expr->rhs);
      expr->type = expr->rhs->type;
    }

    // Handle the specials
    if (expr->op.kind == STAR) {
      if (expr->rhs->type->kind != POINTER_TYPE) {
        failSema("Expected pointer type for *");
      }
      expr->type = expr->type->arg;
    }

    // TODO: correct?
    if (expr->op.kind == BANG) {
      expr->type = newType(INT_TYPE);
    }

    break;

  case SIZEOF_EXPR:
    if (expr->rhs != NULL) {
      semaExprNoDecay(state, expr->rhs);
      expr->value = getSize(state, expr->rhs->type);
    } else {
      expr->value = getSize(state, expr->sizeofArg);
    }
    expr->kind = INT_EXPR;
    expr->type = newType(INT_TYPE);
    break;
  case CAST_EXPR:
    semaExpr(state, expr->lhs);
    if (expr->type == NULL) {
      failSema("Cast without type?");
    }
    break;
  }
}

void semaStmt(struct SemaState *state, struct StmtAST *stmt);

void semaDecl(struct SemaState *state, struct DeclAST *decl) {
  if (decl->name.kind != TOK_EOF) {

    // Allow redef of functions, TODO: verify type match...
    if (decl->kind != FUNC_DECL &&
        findLocal(state->locals, decl->name) != NULL) {
      printToken(decl->name);
      failSema("Variable redef");
    }

    struct VarType *newType = newVarType(decl->name, decl->type);
    newType->next = state->locals;
    state->locals = newType;
  }

  switch (decl->kind) {
  case STRUCT_DECL:
    if (findType(state->types, decl->type->tag) != NULL) {
      failSema("Type redef");
    }

    // Add sub-types to state, but ignore field var decls.
    struct SemaState subState = {0};
    subState.parent = state;
    subState.types = state->types;
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->next) {
      if (field->kind == ENUM_DECL || field->kind == STRUCT_DECL) {
        semaDecl(&subState, field);
      } else if (field->kind != VAR_DECL) {
        failSema("Field must be enum, struct or var.");
      }
    }
    state->types = subState.types;

    struct TypeDef *type = newTypeDef(decl);
    type->next = state->types;
    state->types = type;
    return;

  case ENUM_DECL: {
    // TODO: this is wrong..
    struct SemaState *root = state;
    while (root->parent != NULL) {
      root = root->parent;
    }

    // add a global/root var for reach field.
    int idx = 0;
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->next) {
      struct VarType *newType = newVarType(field->name, NULL);
      newType->enumValue = idx++;
      newType->next = root->locals;
      root->locals = newType;
    }
  } break;

  case FUNC_DECL:
    if (decl->body != NULL) {
      struct SemaState funcState = {0};
      funcState.parent = state;
      funcState.result = decl->type->result;

      // Generate a local for each arg.
      for (struct DeclAST *field = decl->fields; field != NULL;
           field = field->next) {
        struct VarType *newType = newVarType(field->name, field->type);
        newType->next = funcState.locals;
        funcState.locals = newType;
      }
      semaStmt(&funcState, decl->body);
    }
    break;

  case VAR_DECL:

    if (decl->init) {
      if (decl->type->kind == ARRAY_TYPE) {
        semaExprNoDecay(state, decl->init);
      } else {
        semaExpr(state, decl->init);
      }

      if (decl->type->kind == STRUCT_TYPE && decl->init->kind == ARRAY_EXPR) {
        // TODO: verify match?
        if (decl->init->rhs != NULL || decl->init->lhs->kind != INT_EXPR) {
          failSema("Currently only zero init supported");
        }
        decl->init->type = decl->type;
      } else if (decl->init = doConvert(decl->init, decl->type),
                 decl->init == NULL) {
        printDecl(decl);
        failSema(": Decl init type doesn't match");
      }
      if (decl->type->kind == ARRAY_TYPE && decl->type->size == 0) {
        decl->type->size = decl->init->type->size;
      }
    }
    break;
  }
}

void semaStmt(struct SemaState *state, struct StmtAST *stmt) {
  switch (stmt->kind) {
  case EXPR_STMT:
    if (stmt->expr != NULL) {
      semaExpr(state, stmt->expr);
    }
    break;
  case DECL_STMT:
    return semaDecl(state, stmt->decl);

  case RETURN_STMT:
    if (stmt->expr == NULL && state->result->kind != VOID_TYPE) {
      failSema("Return type should be void");
    }
    if (stmt->expr != NULL) {
      semaExpr(state, stmt->expr);
      stmt->expr = doConvert(stmt->expr, state->result);
      if (stmt->expr == NULL) {
        return failSema("Return type mismatch");
      }
    }
    break;

  case COMPOUND_STMT: {
    struct SemaState subState = {0};
    subState.parent = state;
    subState.result = state->result;

    for (struct StmtAST *cur = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      semaStmt(&subState, cur);
    }
  } break;

  case IF_STMT:
    semaExpr(state, stmt->expr);
    checkBool(stmt->expr);

    semaStmt(state, stmt->init);
    if (stmt->stmt) {
      semaStmt(state, stmt->stmt);
    }
    break;

  case WHILE_STMT:
    semaExpr(state, stmt->expr);
    checkBool(stmt->expr);
    semaStmt(state, stmt->stmt);
    break;

  case FOR_STMT: {
    struct SemaState subState = {0};
    subState.parent = state;
    subState.result = state->result;
    semaStmt(&subState, stmt->init);
    // cond must be expr stmt.
    semaExpr(&subState, stmt->cond->expr);
    checkBool(stmt->cond->expr);
    semaExpr(&subState, stmt->expr);

    semaStmt(&subState, stmt->stmt);
  } break;

  case SWITCH_STMT:
    semaExpr(state, stmt->expr);
    // TODO: check if can be converted to int?
    if (stmt->expr->type->kind != INT_TYPE &&
        stmt->expr->type->kind != ENUM_TYPE) {
      printType(stmt->expr->type);
      failSema("Switch expr must be int");
    }
    semaStmt(state, stmt->stmt);

  case CASE_STMT:
    semaExpr(state, stmt->expr);
    if (stmt->expr->type->kind != INT_TYPE &&
        stmt->expr->type->kind != ENUM_TYPE) {
      failSema("case expr must be int");
    }
    break;

  case BREAK_STMT:
  case DEFAULT_STMT:
    break;
  }
}

void semaTopLevel(struct SemaState *state, struct DeclAST *decl) {
  while (decl != NULL) {
    semaDecl(state, decl);
    decl = decl->next;
  }
}

// 3. emit
void failEmit(const char *msg) {
  puts(msg);
  exit(1);
}

// Convert type to LLVM type.
const char *convertType(struct Type *type) {
  switch (type->kind) {
  case VOID_TYPE:
    return "void";
  case CHAR_TYPE:
    return "i8";
  case INT_TYPE:
  case ENUM_TYPE:
    return "i32";
  case POINTER_TYPE:
    return "ptr";
  case STRUCT_TYPE: {
    int len = type->tag.end - type->tag.data;
    char *buf = malloc(len + 10);
    sprintf(buf, "%%struct.%.*s", len, type->tag.data);
    return buf;
  }

  case ARRAY_TYPE: {
    char *buf = malloc(32);
    sprintf(buf, "[%d x %s]", type->size, convertType(type->arg));
    return buf;
  }

  case FUNC_TYPE:
    return "ptr"; // TODO?
  }
  return NULL;
}

struct LocalVar *newLocal(struct Token name, struct Value val) {
  struct LocalVar *local = calloc(1, sizeof(struct LocalVar));
  local->name = name;
  local->value = val;
  return local;
}

struct Value intToVal(int num) {
  struct Value val;
  val.type = "i32";

  char *buf = malloc(16);
  sprintf(buf, "%d", num);
  val.val = buf;

  return val;
}

struct Value getNextTemp(struct EmitState *state) {
  struct Value val;

  char *buf = malloc(16);
  sprintf(buf, "%%tmp%d", state->tmpCounter++);
  val.val = buf;

  return val;
}

struct Value getGlobal(struct Token ident) {
  struct Value val;

  int len = ident.end - ident.data;
  char *buf = malloc(len + 2);
  sprintf(buf, "@%.*s", len, ident.data);
  val.val = buf;

  return val;
}

struct Value getTempGlobal(struct EmitState *state, const char *prefix) {
  struct Value val;

  char *buf = malloc(64);
  sprintf(buf, "@%s%d", prefix, state->tmpCounter++);
  val.val = buf;

  return val;
}

struct Value emitExpr(struct EmitState *state, struct ExprAST *expr);

// Turns an i1 into an i32
struct Value upcasti1(struct EmitState *state, struct Value val) {
  struct Value up = getNextTemp(state);
  up.type = val.type;
  if (strcmp(up.type, "ptr") == 0) {
    failEmit("bool to pointer?");
  }
  printf("  %s = zext i1 %s to %s\n", up.val, val.val, up.type);
  return up;
}

// Turns an i31 into an i1
struct Value makeBool(struct EmitState *state, struct Value val) {
  struct Value up = getNextTemp(state);
  up.type = val.type;
  printf("  %s = icmp ne %s %s, 0\n", up.val, val.type, val.val);
  return up;
}

struct Value lookupVar(struct EmitState *state, struct Token tok) {
  for (struct LocalVar *local = state->vars; local != NULL;
       local = local->next) {
    if (tokCmp(tok, local->name)) {
      return local->value;
    }
  }

  if (state->parent != NULL) {
    return lookupVar(state->parent, tok);
  }

  printToken(tok);
  failEmit("Unkown variable!");
  struct Value v;
  v.val = "undef";
  return v;
}

struct Value emitAddr(struct EmitState *state, struct ExprAST *expr) {
  switch (expr->kind) {
  case VARIABLE_EXPR:
    return lookupVar(state, expr->identifier);

  case INDEX_EXPR: {
    struct Value array = emitExpr(state, expr->lhs);
    struct Value index = emitExpr(state, expr->rhs);

    struct Value gep = getNextTemp(state);
    gep.type = "ptr";

    printf("  %s = getelementptr inbounds %s, ptr %s, %s %s\n", gep.val,
           convertType(expr->lhs->type->arg), array.val, index.type, index.val);
    return gep;
  }

  case MEMBER_EXPR: {
    struct Value agg = expr->op.kind == DOT ? emitAddr(state, expr->lhs)
                                            : emitExpr(state, expr->lhs);
    struct Type *aggType =
        expr->op.kind == DOT ? expr->lhs->type : expr->lhs->type->arg;

    struct Value gep = getNextTemp(state);
    gep.type = "ptr";
    printf("  %s = getelementptr inbounds %s, ptr %s, i32 0, i32 %d\n", gep.val,
           convertType(aggType), agg.val, expr->value);
    return gep;
  }

  case UNARY_EXPR:
    if (expr->op.kind == STAR) {
      return emitExpr(state, expr->rhs);
    }
  case INT_EXPR:
  case BINARY_EXPR:
  case CONDITIONAL_EXPR:
  case SIZEOF_EXPR:
  case STR_EXPR:
  case ARRAY_EXPR:
  case CALL_EXPR:
  case ARG_LIST:
  case CAST_EXPR:
    printExpr(expr);
    failEmit(" Can't be use as lvalue");
    break;
  }

  struct Value v;
  v.val = "undef";
  return v;
}

void emitStore(struct Value addr, struct Value val) {
  printf("  store %s %s, ptr %s\n", val.type, val.val, addr.val);
}

struct Value emitLoad(struct EmitState *state, struct Value addr,
                      const char *type) {
  struct Value val = getNextTemp(state);
  val.type = type;
  printf("  %s = load %s, ptr %s\n", val.val, val.type, addr.val);
  return val;
}

struct Value emitBinary(struct EmitState *state, struct Type *resType,
                        int opKind, struct Value lhs, struct Type *lhsType,
                        struct Value rhs, struct Type *rhsType) {

  if (strcmp(lhs.type, rhs.type) != 0) {
    printf("%s <> %s ", lhs.type, rhs.type);
    failEmit("Lhs and rhs don't have same type!");
  }

  const char *instr;
  int upcast = 0;
  switch (opKind) {
  default:
    failEmit("Invalid binary op");
    break;
  case PLUS:
  case ADD_ASSIGN:
    instr = "add";
    break;
  case MINUS:
  case SUB_ASSIGN:
    instr = "sub";
    break;
  case STAR:
  case MUL_ASSIGN:
    instr = "mul";
    break;
  case SLASH:
  case DIV_ASSIGN:
    instr = "sdiv";
    break;
  case PERCENT:
  case MOD_ASSIGN:
    instr = "srem";
    break;

  case LEFT_OP:
  case LEFT_ASSIGN:
    instr = "shl";
    break;
  case RIGHT_OP:
  case RIGHT_ASSIGN:
    instr = "ashr";
    break;

  case LESS:
    instr = "icmp slt";
    upcast = 1;
    break;
  case GREATER:
    instr = "icmp sgt";
    upcast = 1;
    break;
  case LE_OP:
    instr = "icmp sle";
    upcast = 1;
    break;
  case GE_OP:
    instr = "icmp sge";
    upcast = 1;
    break;
  case EQ_OP:
    instr = "icmp eq";
    upcast = 1;
    break;
  case NE_OP:
    instr = "icmp ne";
    upcast = 1;
    break;

  case AND:
  case AND_ASSIGN:
    instr = "and";
    break;
  case HAT:
  case XOR_ASSIGN:
    instr = "xor";
    break;
  case PIPE:
  case OR_ASSIGN:
    instr = "or";
    break;
  }
  struct Value res = getNextTemp(state);
  res.type = convertType(resType);
  printf("  %s = %s %s %s, %s\n", res.val, instr, lhs.type, lhs.val, rhs.val);

  if (upcast) {
    return upcasti1(state, res);
  }

  return res;
}

struct Value emitAssignment(struct EmitState *state, struct ExprAST *expr) {
  struct Value addr = emitAddr(state, expr->lhs);
  struct Value val = emitExpr(state, expr->rhs);

  if (expr->op.kind == EQ) {
    emitStore(addr, val);
    return val;
  }

  struct Value lval = emitLoad(state, addr, convertType(expr->lhs->type));

  struct Value res = emitBinary(state, expr->type, expr->op.kind, lval,
                                expr->lhs->type, val, expr->rhs->type);

  emitStore(addr, res);
  return res;
}

struct Value emitLogicalBinOp(struct EmitState *state, struct ExprAST *expr) {
  struct Value lhs = emitExpr(state, expr->lhs);
  int idx = state->tmpCounter++;

  const char *firstLabel = "true";
  const char *secondLabel = "false";
  if (expr->op.kind == OR_OP) {
    firstLabel = "false";
    secondLabel = "true";
  }

  struct Value firstCmp = getNextTemp(state);
  printf("  br label %%entry.%d\n", idx);
  printf("entry.%d:\n", idx);
  printf("  %s = icmp ne %s %s, 0\n", firstCmp.val, lhs.type, lhs.val);
  printf("  br i1 %s, label %%%s.%d, label %%%s.%d\n", firstCmp.val, firstLabel,
         idx, secondLabel, idx);

  printf("true.%d:\n", idx);
  struct Value rhs = emitExpr(state, expr->rhs);
  struct Value secondCmp = getNextTemp(state);
  printf("  %s = icmp ne %s %s, 0\n", secondCmp.val, rhs.type, rhs.val);
  printf("  br label %%false.%d\n", idx);

  printf("false.%d:\n", idx);
  struct Value res = getNextTemp(state);
  res.type = convertType(expr->type);
  printf("  %s = phi i1 [ %s, %%entry.%d ], [ %s, %%true.%d ]\n", res.val,
         secondLabel, idx, secondCmp.val, idx);

  return upcasti1(state, res);
}

struct Value emitPtrBinOp(struct EmitState *state, struct ExprAST *expr) {
  if (expr->lhs->type->kind == POINTER_TYPE &&
      expr->rhs->type->kind == POINTER_TYPE && expr->op.kind == MINUS) {
    struct Value lhs = emitExpr(state, expr->lhs);
    struct Value rhs = emitExpr(state, expr->rhs);
    // ptrtoint
    struct Value lhsInt = getNextTemp(state);
    lhsInt.type = convertType(expr->type);
    printf("  %s = ptrtoint %s %s to %s\n", lhsInt.val, lhs.type, lhs.val,
           lhsInt.type);
    // ptrtoint
    struct Value rhsInt = getNextTemp(state);
    rhsInt.type = lhsInt.type;
    printf("  %s = ptrtoint %s %s to %s\n", rhsInt.val, rhs.type, rhs.val,
           rhsInt.type);

    // sub
    struct Value res = getNextTemp(state);
    res.type = lhsInt.type;
    printf("  %s = sub %s %s, %s\n", res.val, res.type, lhsInt.val, rhsInt.val);

    return res;
  }

  struct ExprAST *ptrExpr =
      expr->lhs->type->kind == POINTER_TYPE ? expr->lhs : expr->rhs;
  struct ExprAST *intExpr =
      expr->lhs->type->kind == POINTER_TYPE ? expr->rhs : expr->lhs;

  struct Value ptr = emitExpr(state, ptrExpr);
  struct Value num = emitExpr(state, intExpr);

  // negate num
  if (expr->op.kind == MINUS) {
    struct Value neg = getNextTemp(state);
    neg.type = num.type;
    printf("  %s = sub %s 0, %s\n", neg.val, num.type, num.val);
    num = neg;
  }

  struct Value res = getNextTemp(state);
  res.type = "ptr";
  printf("  %s = getelementptr inbounds %s, %s %s, %s %s\n", res.val,
         convertType(ptrExpr->type->arg), ptr.type, ptr.val, num.type, num.val);
  return res;
}

struct Value emitBinOp(struct EmitState *state, struct ExprAST *expr) {
  if (isAssign(expr->op)) {
    return emitAssignment(state, expr);
  }
  if (expr->op.kind == AND_OP || expr->op.kind == OR_OP) {
    return emitLogicalBinOp(state, expr);
  }

  if ((expr->op.kind == PLUS || expr->op.kind == MINUS ||
       expr->op.kind == ADD_ASSIGN || expr->op.kind == SUB_ASSIGN) &&
      (expr->lhs->type->kind == POINTER_TYPE ||
       expr->rhs->type->kind == POINTER_TYPE)) {
    return emitPtrBinOp(state, expr);
  }

  struct Value lhs = emitExpr(state, expr->lhs);
  struct Value rhs = emitExpr(state, expr->rhs);

  return emitBinary(state, expr->type, expr->op.kind, lhs, expr->lhs->type, rhs,
                    expr->rhs->type);
}

struct Value emitUnary(struct EmitState *state, struct ExprAST *expr) {
  if (expr->op.kind == AND) {
    struct Value res = emitAddr(state, expr->rhs);
    res.type = "ptr";
    return res;
  }

  if (expr->op.kind == INC_OP || expr->op.kind == DEC_OP) {
    struct ExprAST *opExpr = expr->lhs == NULL ? expr->rhs : expr->lhs;
    struct Value operand = emitAddr(state, opExpr);

    struct Value val = emitLoad(state, operand, convertType(opExpr->type));

    const char *op = expr->op.kind == INC_OP ? "add" : "sub";
    struct Value res = getNextTemp(state);
    res.type = val.type;
    printf("  %s = %s %s %s, 1\n", res.val, op, res.type, val.val);
    emitStore(operand, res);

    if (expr->lhs != NULL) {
      return val;
    }
    return res;
  }

  struct Value operand = emitExpr(state, expr->rhs);
  struct Value res = getNextTemp(state);

  const char *instr;
  const char *constop;
  int upcast = 0;
  switch (expr->op.kind) {
  default:
  case INC_OP:
  case DEC_OP:
    failEmit("Invalid unary");
    break;

  case STAR:
    res.type = convertType(expr->type);
    printf("  %s = load %s, ptr %s\n", res.val, res.type, operand.val);
    return res;

  case PLUS:
    return operand;
  case MINUS:
    instr = "sub";
    constop = "0";
    break;
  case TILDE:
    instr = "xor";
    constop = "-1";
    break;
  case BANG:
    instr = "icmp ne";
    constop = "0";
    upcast = 1;
    break;
  }

  res.type = operand.type;
  printf("  %s = %s %s %s, %s\n", res.val, instr, operand.type, constop,
         operand.val);

  if (upcast) {
    return upcasti1(state, res);
  }

  return res;
}

struct Value emitVarRef(struct EmitState *state, struct ExprAST *expr) {
  struct Value addr = emitAddr(state, expr);

  // Funcs and arrays are implictly converted to pointers here.
  if (expr->type->kind == FUNC_TYPE || expr->type->kind == ARRAY_TYPE ||
      (expr->type->kind == POINTER_TYPE && expr->type->isDecay)) {
    addr.type = "ptr";
    return addr;
  }

  struct Value val = getNextTemp(state);
  val.type = convertType(expr->type);
  printf("  %s = load %s, ptr %s\n", val.val, val.type, addr.val);
  return val;
}

struct Value getStrConst(struct Type *type, struct Token tok) {
  int len = tok.end - tok.data;
  char *val = malloc(len + 16);
  sprintf(val, "c\"%.*s\\00\"", len, tok.data);

  struct Value res;
  res.type = convertType(type);
  res.val = val;
  return res;
}

struct Value emitStrRef(struct EmitState *state, struct ExprAST *expr) {

  struct Value strGlobal = getTempGlobal(state, "str.");
  strGlobal.type = "ptr";

  struct Type *strType = newType(ARRAY_TYPE);
  strType->arg = expr->type->arg;
  strType->size = expr->identifier.end - expr->identifier.data + 1;
  struct Value strConst = getStrConst(strType, expr->identifier);

  printf("%s = constant %s %s\n", strGlobal.val, strConst.type, strConst.val);

  return strGlobal;
}

struct Value emitArray(struct EmitState *state, struct ExprAST *expr) {
  struct Value res;
  res.type = convertType(expr->type);
  if (expr->type->kind == STRUCT_TYPE) {
    res.val = "zeroinitializer";
    return res;
  }

  char *buf = malloc(64 * expr->type->size);
  res.val = buf;

  buf += sprintf(buf, "[ ");

  for (struct ExprAST *field = expr; field != NULL; field = field->rhs) {
    struct Value elem = emitExpr(state, field->lhs);
    buf += sprintf(buf, "%s %s", elem.type, elem.val);
    if (field->rhs != NULL) {
      buf += sprintf(buf, ", ");
    }
  }
  buf += sprintf(buf, " ]");

  return res;
}

struct Value emitCall(struct EmitState *state, struct ExprAST *expr) {
  struct LocalVar *args = NULL;
  struct LocalVar *argsTail = NULL;

  for (struct ExprAST *arg = expr->rhs; arg != NULL; arg = arg->rhs) {
    struct Token eof;
    struct Value argVal = emitExpr(state, arg->lhs);

    struct LocalVar *nextArg = newLocal(eof, argVal);
    if (args == NULL) {
      args = nextArg;
    } else {
      argsTail->next = nextArg;
    }
    argsTail = nextArg;
  }
  struct Value fn = emitExpr(state, expr->lhs);

  struct Value res = getNextTemp(state);
  res.type = convertType(expr->type);
  printf("  %s = call %s %s(", res.val, res.type, fn.val);
  for (; args != NULL; args = args->next) {
    printf("%s %s", args->value.type, args->value.val);
    if (args->next != NULL) {
      printf(", ");
    }
  }

  printf(")\n");

  return res;
}

struct Value emitMemberOrIndex(struct EmitState *state, struct ExprAST *expr) {
  struct Value addr = emitAddr(state, expr);
  return emitLoad(state, addr, convertType(expr->type));
}

struct Value emitCast(struct EmitState *state, struct ExprAST *expr) {
  struct Value v = emitExpr(state, expr->lhs);

  struct Type *from = expr->lhs->type;
  struct Type *to = expr->type;
  if (from->kind == POINTER_TYPE && to->kind == POINTER_TYPE) {
    return v;
  }

  struct Value res = getNextTemp(state);
  res.type = convertType(to);

  if (from->kind == INT_TYPE && to->kind == CHAR_TYPE) {
    printf("  %s = trunc %s %s to %s\n", res.val, v.type, v.val, res.type);
  } else if (from->kind == CHAR_TYPE && to->kind == INT_TYPE) {
    printf("  %s = zext %s %s to %s\n", res.val, v.type, v.val, res.type);
  } else {
    failEmit("Not supported!");
  }

  return res;
}

struct Value emitCond(struct EmitState *state, struct ExprAST *expr) {
  struct Value cond = emitExpr(state, expr->cond);
  cond = makeBool(state, cond);

  const char *falseLabel = "false";
  int idx = state->tmpCounter++;
  printf("  br i1 %s, label %%cond.true.%d, label %%cond.%s.%d\n", cond.val,
         idx, falseLabel, idx);

  printf("cond.true.%d:\n", idx);
  struct Value trueVal = emitExpr(state, expr->lhs);
  printf("  br label %%cond.cont.%d\n", idx);

  printf("cond.false.%d:\n", idx);
  struct Value falseVal = emitExpr(state, expr->rhs);
  printf("  br label %%cond.cont.%d\n", idx);

  printf("cond.cont.%d:\n", idx);

  struct Value res = getNextTemp(state);
  res.type = trueVal.type;
  printf("  %s = phi %s [ %s, %%cond.true.%d ], [ %s, %%cond.false.%d ]\n",
         res.val, res.type, trueVal.val, idx, falseVal.val, idx);
  return res;
}
// \returns The register name or value of the expr
struct Value emitExpr(struct EmitState *state, struct ExprAST *expr) {
  switch (expr->kind) {
  case INT_EXPR:
    return intToVal(expr->value);
  case BINARY_EXPR:
    return emitBinOp(state, expr);
  case UNARY_EXPR:
    return emitUnary(state, expr);
  case VARIABLE_EXPR:
    return emitVarRef(state, expr);

  case STR_EXPR:
    if (expr->type->kind == ARRAY_TYPE) {
      return getStrConst(expr->type, expr->identifier);
    }
    return emitStrRef(state, expr);

  case ARRAY_EXPR:
    return emitArray(state, expr);

  case CALL_EXPR:
    return emitCall(state, expr);

  case INDEX_EXPR:
  case MEMBER_EXPR:
    return emitMemberOrIndex(state, expr);

  case CAST_EXPR:
    return emitCast(state, expr);

  case CONDITIONAL_EXPR:
    return emitCond(state, expr);

  case SIZEOF_EXPR:
    printExpr(expr);

  case ARG_LIST:
    failEmit("Unsupported expr");
  }

  struct Value v;
  v.type = "i32";
  v.val = "undef";
  return v;
}

void emitReturn(struct EmitState *state, struct StmtAST *stmt) {
  if (stmt->expr) {
    struct Value v = emitExpr(state, stmt->expr);
    printf("  ret %s %s\n", v.type, v.val);
    return;
  }

  printf("  ret void\n");
}

void addLocal(struct EmitState *state, struct Token name, struct Value val) {
  struct LocalVar *local = newLocal(name, val);
  local->next = state->vars;
  state->vars = local;
}

struct Value emitLocalVar(struct EmitState *state, struct DeclAST *decl) {
  struct Value val = getNextTemp(state);
  val.type = "ptr";

  const char *type = convertType(decl->type);
  printf("  %s = alloca %s\n", val.val, type);

  struct LocalVar *local = newLocal(decl->name, val);
  local->next = state->vars;
  state->vars = local;

  if (decl->init) {
    struct Value init = emitExpr(state, decl->init);
    emitStore(val, init);
  }

  return val;
}

void emitLocalDecl(struct EmitState *state, struct DeclAST *decl) {
  switch (decl->kind) {
  case VAR_DECL:
  case ENUM_DECL:
    emitLocalVar(state, decl);
    break;
  case STRUCT_DECL:
  case FUNC_DECL:
    failEmit("Unsupported");
  }
}

void emitStmt(struct EmitState *state, struct StmtAST *stmt);

void emitIf(struct EmitState *state, struct StmtAST *stmt) {
  struct Value cond = emitExpr(state, stmt->expr);
  cond = makeBool(state, cond);

  const char *falseLabel = "false";
  if (stmt->stmt == NULL) {
    falseLabel = "cont";
  }
  int idx = state->tmpCounter++;
  printf("  br i1 %s, label %%if.true.%d, label %%if.%s.%d\n", cond.val, idx,
         falseLabel, idx);

  printf("if.true.%d:\n", idx);
  emitStmt(state, stmt->init);
  printf("  br label %%if.cont.%d\n", idx);

  if (stmt->stmt) {
    printf("if.false.%d:\n", idx);
    emitStmt(state, stmt->stmt);
    printf("  br label %%if.cont.%d\n", idx);
  }

  printf("if.cont.%d:\n", idx);
}

void emitWhile(struct EmitState *state, struct StmtAST *stmt) {
  int idx = state->tmpCounter++;

  printf("  br label %%while.cond.%d\n", idx);
  printf("while.cond.%d:\n", idx);
  struct Value cond = makeBool(state, emitExpr(state, stmt->expr));
  printf("  br i1 %s, label %%while.body.%d, label %%while.cont.%d\n", cond.val,
         idx, idx);

  printf("while.body.%d:\n", idx);
  emitStmt(state, stmt->stmt);
  printf(" br label %%while.cond.%d\n", idx);

  printf("while.cont.%d:\n", idx);
}

void emitFor(struct EmitState *state, struct StmtAST *stmt) {
  int idx = state->tmpCounter++;

  emitStmt(state, stmt->init);

  printf("  br label %%for.cond.%d\n", idx);
  printf("for.cond.%d:\n", idx);
  // cond must be an expression stmt, parseFor guarantees it.
  struct Value cond = makeBool(state, emitExpr(state, stmt->cond->expr));
  printf("  br i1 %s, label %%for.body.%d, label %%for.cont.%d\n", cond.val,
         idx, idx);

  printf("for.body.%d:\n", idx);
  emitStmt(state, stmt->stmt);
  printf("  br label %%for.incr.%d\n", idx);

  // TODO: continue would jump here
  printf("for.incr.%d:\n", idx);
  emitExpr(state, stmt->expr);
  printf("  br label %%for.cond.%d\n", idx);

  printf("for.cont.%d:\n", idx);
}

void emitSwitch(struct EmitState *state, struct StmtAST *stmt) {
  int idx = state->tmpCounter++;

  struct EmitState switchState = {0};
  switchState.tmpCounter = state->tmpCounter;
  switchState.parent = state;

  char *buf = malloc(32);
  sprintf(buf, "cont.%d", idx);
  switchState.curBreakLabel = buf;

  struct Value expr = emitExpr(&switchState, stmt->expr);
  printf("  br label %%switch.%d\n", idx);

  emitStmt(&switchState, stmt->stmt);

  printf("switch.%d:\n", idx);

  if (switchState.defaultLabel != NULL) {
    printf("  switch %s %s, label %%%s [\n", expr.type, expr.val,
           switchState.defaultLabel);
  } else {
    printf("  switch %s %s, label %%cont.%d [\n", expr.type, expr.val, idx);
  }
  for (struct Case *cse = switchState.cases; cse != NULL; cse = cse->next) {
    printf("    %s %s, label %%case.%d\n", cse->val.type, cse->val.val, cse->n);
  }
  printf("  ]\n");
  printf("cont.%d:\n", idx);

  state->tmpCounter = switchState.tmpCounter;
}

void emitCase(struct EmitState *state, struct StmtAST *stmt) {
  int index = state->tmpCounter++;

  // fallthrough.
  printf("  br label %%case.%d\n", index);
  printf("case.%d:\n", index);

  struct Value constExpr = emitExpr(state, stmt->expr);
  struct Case *cse = calloc(1, sizeof(struct Case));
  cse->n = index;
  cse->val = constExpr;
  cse->next = state->cases;
  state->cases = cse;
}

void emitStmt(struct EmitState *state, struct StmtAST *stmt) {
  switch (stmt->kind) {
  case EXPR_STMT:
    if (stmt->expr) {
      emitExpr(state, stmt->expr);
    }
    break;
  case RETURN_STMT:
    emitReturn(state, stmt);
    break;

  case DECL_STMT:
    emitLocalDecl(state, stmt->decl);
    break;

  case COMPOUND_STMT:
    // TODO: new state?
    for (struct StmtAST *cur = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      emitStmt(state, cur);
    }
    break;

  case IF_STMT:
    return emitIf(state, stmt);

  case WHILE_STMT:
    return emitWhile(state, stmt);
  case FOR_STMT:
    return emitFor(state, stmt);

  case SWITCH_STMT:
    return emitSwitch(state, stmt);

  case CASE_STMT:
    return emitCase(state, stmt);

  case BREAK_STMT:
    printf("  br label %%%s\n", state->curBreakLabel);
    break;

  case DEFAULT_STMT:
    if (state->defaultLabel != NULL) {
      failEmit("Multiple default");
    }
    int idx = state->tmpCounter++;
    state->defaultLabel = malloc(32);
    sprintf(state->defaultLabel, "default.%d", idx);
    printf("  br label %%default.%d\n", idx);
    printf("default.%d:\n", idx);
    break;
  }
}

void emitFunc(struct EmitState *state, struct DeclAST *decl) {
  struct Value val = getGlobal(decl->name);
  val.type = "ptr";

  addLocal(state, decl->name, val);

  const char *defOrDecl = decl->body == NULL ? "declare" : "define";
  printf("%s %s %s(", defOrDecl, convertType(decl->type->result), val.val);
  for (struct DeclAST *arg = decl->fields; arg != NULL; arg = arg->next) {
    int len = arg->name.end - arg->name.data;
    printf("%s %%%.*s", convertType(arg->type), len, arg->name.data);
    if (arg->next != NULL) {
      printf(", ");
    }
  }
  printf(")");

  if (decl->body != NULL) {
    struct EmitState funcState = {0};
    funcState.parent = state;
    funcState.tmpCounter = 0;

    printf(" {\n");

    for (struct DeclAST *arg = decl->fields; arg != NULL; arg = arg->next) {
      struct Value addr = emitLocalVar(&funcState, arg);
      int len = arg->name.end - arg->name.data;
      printf("  store %s %%%.*s, ptr %s\n", convertType(arg->type), len,
             arg->name.data, addr.val);
    }
    emitStmt(&funcState, decl->body);

    // Emit implict void return.
    if (decl->type->result->kind == VOID_TYPE) {
      printf("  ret void\n");
    }

    printf("}\n");
  } else {
    printf("\n");
  }
}

void emitStruct(struct EmitState *state, struct DeclAST *decl) {
  // emit nested structs
  for (struct DeclAST *field = decl->fields; field != NULL;
       field = field->next) {
    if (field->kind == STRUCT_DECL) {
      emitStruct(state, field);
    }
  }

  // TODO: padding
  printf("%s = type <{ ", convertType(decl->type));

  for (struct DeclAST *field = decl->fields; field != NULL;
       field = field->next) {
    printf("%s", convertType(field->type));
    if (field->next != NULL) {
      printf(", ");
    }
  }

  printf(" }>\n");
}

void emitGlobalVar(struct EmitState *state, struct DeclAST *decl) {
  const char *declSpec = decl->type->isConst ? "constant" : "global";

  struct Value val = getGlobal(decl->name);
  val.type = convertType(decl->type);
  if (decl->init) {
    struct Value init = emitExpr(state, decl->init); // TODO: emit constant
    printf("%s = %s %s %s\n", val.val, declSpec, init.type, init.val);
  } else {
    const char *init =
        decl->type->kind == STRUCT_TYPE ? "zeroinitializer" : "null";
    printf("%s = %s %s %s\n", val.val, declSpec, val.type, init);
  }

  addLocal(state, decl->name, val);
}

void emitGlobalDecl(struct EmitState *state, struct DeclAST *decl) {
  switch (decl->kind) {
  case ENUM_DECL:
  case VAR_DECL:
    emitGlobalVar(state, decl);
    break;
  case STRUCT_DECL:
    emitStruct(state, decl);
    break;
  case FUNC_DECL:
    emitFunc(state, decl);
    break;
  }
}

void emitTopLevel(struct EmitState *state, struct DeclAST *decl) {
  // TODO: emit types, globals, func decls then func defs?

  while (decl != NULL) {
    emitGlobalDecl(state, decl);
    decl = decl->next;
  }
}

int main(int argc, char **argv) {
  if (argc != 2) {
    puts("Usage: compile file.c");
    return -1;
  }

  int fd = open(argv[1], 0); //  O_RDONLY
  if (fd == -1) {
    return -1;
  }

  int size = lseek(fd, 0, 2); //  SEEK_END
  if (size == -1) {
    return -1;
  }

  if (lseek(fd, 0, 0) == -1) { // SEEK_SET
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

  struct ParseState parseState;
  parseState.current = parseState.start = fileMem;
  parseState.end = fileMem + size;

  struct DeclAST *decls = parseTopLevel(&parseState);

  struct SemaState semaState = {0};
  struct Token nullTok;
  nullTok.kind = IDENTIFIER;
  nullTok.data = "NULL";
  nullTok.end = nullTok.data + 4;

  // Add null as a nullptr
  semaState.locals = newVarType(nullTok, newType(POINTER_TYPE));
  semaState.locals->type->arg = newType(VOID_TYPE);

  semaTopLevel(&semaState, decls);

  struct EmitState emitState = {0};
  struct Value nullVal;
  nullVal.type = "ptr";
  nullVal.val = "null";
  emitState.vars = newLocal(nullTok, nullVal);

  emitTopLevel(&emitState, decls);

  return 0;
}
