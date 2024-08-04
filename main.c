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
  enum {
    TOK_EOF,

    IDENT,    // [data, end[ is value
    INTEGER,  // number has value
    FLOAT,    // floating has value
    STRING,   // [data, end[ is value
    CHAR,     // [data, end[ is value
    OPERATOR, // [data, end[ is value (=, ==, ...)

    // keywords
    IF,
    // TYPEDEF,
    // STRUCT,
    // RETURN,
    // ELSE,
    // WHILE,
    // FOR,

  } type;

  char *data;
  char *end;

  long long number;
  double floating;
};

struct ParseState {
  char *start;
  char *end;

  char *current;

  struct Token curToken;
};

struct ExprAST {
  enum {
    INT_EXPR,
    BINARY_EXPR,
    VARIABLE_EXPR,
    CALL_EXPR,
  } type;

  // int
  long long value;

  // binary
  char binop;
  struct ExprAST *lhs;
  struct ExprAST *rhs;

  // variable
  char *start;
  char *end;

  // call
  struct ExprAST *callee;
};

/// debug only:
/// \{
void printToken(struct Token token) {
  switch (token.type) {
  case TOK_EOF:
    return;

  case INTEGER:
    printf("INTEGER(");
    printf("%lld", token.number);
    printf(") ");
    return;
  case FLOAT:
    printf("FLOAT(");
    printf("%f", token.floating);
    printf(") ");
    return;

  case IF:
    printf("IF ");
    return;
    // case TYPEDEF:
    //   printf("TYPEDEF ");
    //   return;
    // case STRUCT:
    //   printf("STRUCT ");
    //   return;
    // case RETURN:
    //   printf("RETURN ");
    //   return;
    // case ELSE:
    //   printf("ELSE ");
    //   return;
    // case WHILE:
    //   printf("WHILE ");
    //   return;
    // case FOR:
    //   printf("FOR ");
    //   return;

  case OPERATOR:
    if (*token.data == ';') {
      printf("OP(;)\n");
      return;
    }
    if (*token.data == '}') {
      printf("OP(})\n");
      return;
    }
    if (*token.data == '{') {
      printf("OP({)\n");
      return;
    }
    printf("OP");
    break;
  case STRING:
    printf("STRING");
    break;
  case CHAR:
    printf("CHAR");
    break;
  case IDENT:
    printf("IDENT");
    break;
  }
  printf("(");
  for (char *c = token.data; c != token.end; c++) {
    putchar(*c);
  }
  printf(") ");
}

void printExpr(struct ExprAST *expr) {
  if (expr == NULL) {
    puts("ERROR! null expr");
    return;
  }
  switch (expr->type) {
  case VARIABLE_EXPR:
    for (char *c = expr->start; c != expr->end; c++) {
      putchar(*c);
    }
    break;
  case INT_EXPR:
    printf("INT(%lld)", expr->value);
    break;
  case BINARY_EXPR:
    printf("%c(", expr->binop);
    printExpr(expr->lhs);
    printf(" ");
    printExpr(expr->rhs);
    printf(")");
  }
}

/// end debug only
/// \}

int nextChar(struct ParseState *state) {
  if (state->current == state->end) {
    return EOF;
  }

  int result = *state->current;
  state->current++;
  return result;
}

int peekChar(struct ParseState *state) {
  if (state->current == state->end) {
    return EOF;
  }
  return *state->current;
}

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

    // TODO: all keywords
    if (memcmp(token.data, "if", 2) == 0) {
      token.type = IF;
      return token;
    }

    token.type = IDENT;
    return token;
  }

  if (lastChar == '\'') {
    while (peekChar(state) != '\'') {
      int next = nextChar(state);
      if (next == '\\') {
        nextChar(state);
      }
    }
    token.data = tokenStart + 1; // eat the starting '
    token.end = state->current;

    nextChar(state); // eat closing '
    token.type = CHAR;
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
    token.type = STRING;
    return token;
  }

  if (isdigit(lastChar) || (lastChar == '-' && isdigit(peekChar(state)))) {
    while (isdigit(peekChar(state)) || peekChar(state) == '.') {
      nextChar(state);
    }
    char *endp = state->current;
    token.number = strtol(tokenStart, &endp, 10);
    token.type = INTEGER;
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
  }
  if (lastChar == '-' && peekChar(state) == '>') {
    nextChar(state);
  }
  if (lastChar == '|' && peekChar(state) == '|') {
    nextChar(state);
  }
  if (lastChar == '&' && peekChar(state) == '&') {
    nextChar(state);
  }
  if (lastChar == '!' && peekChar(state) == '=') {
    nextChar(state);
  }
  if (lastChar == '+' && peekChar(state) == '+') {
    nextChar(state);
  }
  if (lastChar == '+' && peekChar(state) == '=') {
    nextChar(state);
  }

  token.type = OPERATOR;
  token.data = tokenStart;
  token.end = state->current;

  return token;
}

struct Token getNextToken(struct ParseState *state) {
  return state->curToken = getToken(state);
}

struct ExprAST *newExpr(int type) {
  struct ExprAST *result = malloc(sizeof(struct ExprAST));
  result->type = type;
  return result;
}

struct ExprAST *parseNumber(struct ParseState *state) {
  struct ExprAST *result = newExpr(INT_EXPR);
  result->value = state->curToken.number;
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

  if (state->curToken.type != OPERATOR || *state->curToken.data != ')') {
    puts("Expected ')'");
    return NULL;
  }

  getNextToken(state); // eat )
  return expr;
}

struct ExprAST *parseIdentifier(struct ParseState *state) {
  struct ExprAST *result = newExpr(VARIABLE_EXPR);
  result->start = state->curToken.data;
  result->end = state->curToken.end;
  getNextToken(state);
  return result;
}

struct ExprAST *parsePrimary(struct ParseState *state) {
  switch (state->curToken.type) {
  case IDENT:
    return parseIdentifier(state);
  case INTEGER:
    return parseNumber(state);
  case OPERATOR:
    if (*state->curToken.data == '(') {
      return parseParen(state);
    }
  default:
    puts("Unknow primary expression");
    return NULL;
  }
}

struct ExprAST *parsePostfix(struct ParseState *state) {
  struct ExprAST *primary = parsePrimary(state);

  while (1) {
    if (*state->curToken.data == '(') {
    }
  }

  return primary;
}

int getPrecedence(struct Token op) {
  if (op.type != OPERATOR) {
    return -1;
  }

  switch (*op.data) {
  case '+':
  case '-':
    return 20;

  case '*':
  case '/':
    return 40;

  default:
    return -1;
  }
}

struct ExprAST *parseBinOp(struct ParseState *state, int prec,
                           struct ExprAST *lhs) {
  while (1) {
    int curPred = getPrecedence(state->curToken);
    if (curPred < prec) {
      return lhs;
    }

    char op = *state->curToken.data;
    getNextToken(state);

    struct ExprAST *rhs = parsePostfix(state);
    if (rhs == NULL) {
      return NULL;
    }

    int nextPred = getPrecedence(state->curToken);
    if (curPred < nextPred) {
      rhs = parseBinOp(state, curPred + 1, rhs);
      if (rhs == NULL) {
        return NULL;
      }
    }

    struct ExprAST *newLhs = newExpr(BINARY_EXPR);
    newLhs->binop = op;
    newLhs->lhs = lhs;
    newLhs->rhs = rhs;

    lhs = newLhs;
  }
}

struct ExprAST *parseExpression(struct ParseState *state) {
  struct ExprAST *lhs = parsePostfix(state);

  if (lhs == NULL) {
    return NULL;
  }

  return parseBinOp(state, 0, lhs);
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
    struct ExprAST *expr = parseExpression(&state);
    printExpr(expr);
    if (expr == NULL) {
      return -1;
    }
  }

  // No cleanup needed
  return 0;
}
