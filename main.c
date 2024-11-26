#include <ctype.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// 1. parse
// 2. AST
// 3. emit

struct Token {
  enum Type {
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
  } type;

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

struct ExprAST {
  enum {
    INT_EXPR,      // 1
    STR_EXPR,      // "foo"
    VARIABLE_EXPR, // foo

    CALL_EXPR,   // lhs()
    INDEX_EXPR,  // lhs[rhs]
    MEMBER_EXPR, // primary.identifier or primary->identifier based on op
    UNARY_EXPR,  // lhs++, lhs-- or --rhs ++rhs based on op
    SIZEOF_EXPR, // sizeof rhs

    CONDITIONAL_EXPR, // cond ? lhs : rhs

    ARG_LIST, // lhs, rhs

    BINARY_EXPR, // a + b, ...
  } type;

  // primary_expr
  // \{
  // int
  long long value;
  // \}

  // binary
  struct Token op;
  struct ExprAST *lhs;
  struct ExprAST *rhs;

  struct Token identifier;

  struct ExprAST *cond;
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
  printf("%s", token_str[token.type]);
  printf("(");
  printStr(token.data, token.end);
  printf(") ");
}

void printExpr(struct ExprAST *expr) {
  if (expr == NULL) {
    puts("ERROR! null expr");
    return;
  }

  switch (expr->type) {
  case VARIABLE_EXPR:
    printToken(expr->identifier);
    break;
  case INT_EXPR:
    printf("INT(%lld)", expr->value);
    break;
  case STR_EXPR:
    printf("STR(");
    printStr(expr->identifier.data, expr->identifier.end);
    printf(")");
    break;
  case BINARY_EXPR:
    printf("%s(", token_str[expr->op.type]);
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
    printf(" %s ", token_str[expr->op.type]);
    printStr(expr->identifier.data, expr->identifier.end);
    printf(")");
    break;
  case UNARY_EXPR:
    printf("UNARY(");
    if (expr->lhs) {
      printExpr(expr->lhs);
    }
    printf("%s", token_str[expr->op.type]);
    if (expr->rhs) {
      printExpr(expr->rhs);
    }
    printf(")");
    break;
  case SIZEOF_EXPR:
    printf("SIZEOF(");
    printExpr(expr->rhs);
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
  }
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
    token.type = TOK_EOF;
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
        token.type = SIZEOF + i;
        return token;
      }
    }

    token.type = IDENTIFIER;
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
    token.type = CONSTANT;
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
    token.type = STRING_LITERAL;
    return token;
  }

  if (isdigit(lastChar) || (lastChar == '-' && isdigit(peekChar(state)))) {
    while (isdigit(peekChar(state)) || peekChar(state) == '.') {
      nextChar(state);
    }
    token.data = tokenStart;
    token.end = state->current;
    token.type = CONSTANT;
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
    token.type = EQ_OP;
  } else if (lastChar == '-' && peekChar(state) == '>') {
    nextChar(state);
    token.type = PTR_OP;
  } else if (lastChar == '|' && peekChar(state) == '|') {
    nextChar(state);
    token.type = OR_OP;
  } else if (lastChar == '&' && peekChar(state) == '&') {
    nextChar(state);
    token.type = AND_OP;
  } else if (lastChar == '!' && peekChar(state) == '=') {
    nextChar(state);
    token.type = NE_OP;
  } else if (lastChar == '+' && peekChar(state) == '+') {
    nextChar(state);
    token.type = INC_OP;
  } else if (lastChar == '-' && peekChar(state) == '-') {
    nextChar(state);
    token.type = DEC_OP;
  } else if (lastChar == '+' && peekChar(state) == '=') {
    nextChar(state);
    token.type = ADD_ASSIGN;
  } else {
    switch (lastChar) {
      // clang-format off
      case ';': token.type = SEMICOLON; break;
      case '{': token.type = OPEN_BRACE; break;
      case '}': token.type = CLOSE_BRACE; break;
      case ':': token.type = COLON; break;
      case '=': token.type = EQ; break;
      case '(': token.type = OPEN_PAREN; break;
      case ')': token.type = CLOSE_PAREN; break;
      case '[': token.type = OPEN_BRACKET; break;
      case ']': token.type = CLOSE_BRACKET; break;
      case '.': token.type = DOT; break;
      case '&': token.type = AND; break;
      case '!': token.type = BANG; break;
      case '~': token.type = TILDE; break;
      case '-': token.type = MINUS; break;
      case '+': token.type = PLUS; break;
      case '*': token.type = STAR; break;
      case '/': token.type = SLASH; break;
      case '%': token.type = PERCENT; break;
      case '<': token.type = LESS; break;
      case '>': token.type = GREATER; break;
      case '^': token.type = HAT; break;
      case '|': token.type = PIPE; break;
      case '?': token.type = QUESTION; break;
      case ',': token.type = COMMA; break;
      default:
        printf("Unknown token! %c", lastChar);
        exit(EXIT_FAILURE);
      // clang-format on
    }
  }

  token.data = tokenStart;
  token.end = state->current;

  return token;
}

int match(struct ParseState *state, enum Type tok) {
  return state->curToken.type == tok;
}

struct Token getNextToken(struct ParseState *state) {
  struct Token result = state->curToken;
  state->curToken = getToken(state);
  return result;
}

struct ExprAST *newExpr(int type) {
  struct ExprAST *result = malloc(sizeof(struct ExprAST));
  result->type = type;
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
    long long num = strtol(start, &endp, 10);
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
  if (expr == NULL) {
    return NULL;
  }

  if (!match(state, CLOSE_PAREN)) {
    puts("Expected ')'");
    return NULL;
  }

  getNextToken(state); // eat )
  return expr;
}

struct ExprAST *parsePrimary(struct ParseState *state) {
  switch (state->curToken.type) {
  case IDENTIFIER:
    return parseIdentifier(state);
  case CONSTANT:
    return parseNumber(state);
  case STRING_LITERAL:
    return parseString(state);
  case OPEN_PAREN:
    return parseParen(state);
  default:
    puts("Unknow primary expression");
    return NULL;
  }
}

struct ExprAST *parseIndex(struct ParseState *state, struct ExprAST *lhs) {
  struct ExprAST *expr = newExpr(INDEX_EXPR);
  expr->lhs = lhs;

  expr->rhs = parseExpression(state);
  if (expr->rhs == NULL) {
    return NULL;
  }

  if (!match(state, CLOSE_BRACKET)) {
    puts("Expected ]");
    return NULL;
  }
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

    if (cur->lhs == NULL) {
      return NULL;
    }

    if (!match(state, COMMA)) {
      break;
    }
    getNextToken(state);
  }

  if (!match(state, CLOSE_PAREN)) {
    puts("expected , or )");
    return NULL;
  }
  getNextToken(state);

  return expr;
}

struct ExprAST *parseMember(struct ParseState *state, struct ExprAST *lhs) {
  struct ExprAST *expr = newExpr(MEMBER_EXPR);
  expr->lhs = lhs;

  expr->op = state->curToken;
  getNextToken(state);

  if (!match(state, IDENTIFIER)) {
    puts("Expected identifier");
    return NULL;
  }

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

    switch (state->curToken.type) {
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
    expr->lhs = NULL;
    expr->rhs = parseUnary(state);
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
  switch (tok.type) {
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
    if (rhs == NULL) {
      return NULL;
    }

    int nextPred = getPrecedence(state->curToken);
    if (curPred < nextPred) {
      rhs = parseBinOpRhs(state, curPred + 1, rhs);
      if (rhs == NULL) {
        return NULL;
      }
    }

    struct ExprAST *newLhs = newExpr(BINARY_EXPR);
    newLhs->op = op;
    newLhs->lhs = lhs;
    newLhs->rhs = rhs;

    lhs = newLhs;
  }
}

struct ExprAST *parseBinOp(struct ParseState *state) {
  struct ExprAST *lhs = parsePostfix(state);

  if (lhs == NULL) {
    return NULL;
  }

  return parseBinOpRhs(state, 0, lhs);
}

struct ExprAST *parseConditional(struct ParseState *state) {
  struct ExprAST *cond = parseBinOp(state);
  if (!match(state, QUESTION)) {
    return cond;
  }
  getNextToken(state);

  struct ExprAST *trueBranch = parseExpression(state);
  if (!match(state, COLON)) {
    puts("Expected :");
    return NULL;
  }
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

int main(int argc, char **argv) {
  if (argc != 2) {
    puts("Usage: compile file.c");
    return -1;
  }

  int fd = open(argv[1], O_RDONLY);
  if (fd == -1) {
    return -1;
  }
  off_t size = lseek(fd, 0, SEEK_END);
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

  while (state.curToken.type != TOK_EOF) {
    // printToken(state.curToken);
    // getNextToken(&state);
    struct ExprAST *expr = parseExpression(&state);
    printExpr(expr);
    if (expr == NULL) {
      return -1;
    }
  }

  // No cleanup needed
  return 0;
}
