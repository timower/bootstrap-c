import libc;

enum TokenKind {
  TOK_EOF,

  // clang-format off
  // constants
  IDENTIFIER, CONSTANT, STRING_LITERAL, INT2,

  // keywords
  CONTINUE, DEFAULT, SIZEOF, STRUCT, SWITCH, RETURN, IMPORT, CONST,
  WHILE,    BREAK,   VOID,   ENUM,   CASE,   ELSE,

  // operators
  LEFT_ASSIGN, RIGHT_ASSIGN, ELLIPSIS,  FOR, SCOPE,  PTR_OP, INC_OP, DEC_OP,
  LEFT_OP, RIGHT_OP, LE_OP, GE_OP, EQ_OP, NE_OP, AND_OP, OR_OP, MUL_ASSIGN,
  DIV_ASSIGN, MOD_ASSIGN, ADD_ASSIGN, SUB_ASSIGN, AND_ASSIGN, XOR_ASSIGN,
  OR_ASSIGN, IF, AS, SEMICOLON, OPEN_BRACE, CLOSE_BRACE, COMMA, COLON, EQ,
  OPEN_PAREN, CLOSE_PAREN, OPEN_BRACKET, CLOSE_BRACKET, DOT, AND, BANG,
  TILDE, MINUS, PLUS, STAR, SLASH, PERCENT, LESS, GREATER, HAT, PIPE,
  QUESTION
  // clang-format on
};

struct Token {
  enum TokenKind kind;

  i8 *data;
  i8 *end;
};

const i8 *tokens[] = {
    "EOF",      "IDENT",   "CONST",  "STR",    "INT",

    "continue", "default", "sizeof", "struct", "switch", "return", "import",
    "const",    "while",   "break",  "void",   "enum",   "case",   "else",
    "<<=",      ">>=",     "...",    "for",    "::",     "->",     "++",
    "--",       "<<",      ">>",     "<=",     ">=",     "==",     "!=",
    "&&",       "||",      "*=",     "/=",     "%=",     "+=",     "-=",
    "&=",       "^=",      "|=",     "if",     "as",     ";",      "{",
    "}",        ",",       ":",      "=",      "(",      ")",      "[",
    "]",        ".",       "&",      "!",      "~",      "-",      "+",
    "*",        "/",       "%",      "<",      ">",      "^",      "|",
    "?",
};

enum ExprKind {
  INT,      // value
  STR,      // "identifier"
  VARIABLE, // identifier

  ARRAY, // {a, b, c}

  CALL,  // lhs(rhs->lhs, rhs->rhs->lhs, ..)
  INDEX, // lhs[rhs]

  MEMBER, // lhs.identifier, lhs->identifier,
  SCOPE,  // parent::identifier

  UNARY,  // lhs++, lhs-- or --rhs ++rhs based on op
  SIZEOF, // sizeof(rhs) or sizeof(sizeofArg)

  CONDITIONAL, // cond ? lhs : rhs

  ARG_LIST, // lhs, rhs

  BINARY, // a + b, ...

  CAST, // (type)lhs
};

// Represents an expression in the AST.
struct ExprAST {
  enum ExprKind kind;

  struct Type *type;

  // primary_expr
  // \{
  // int
  i32 value;
  // \}

  // binary
  struct Token op;
  struct ExprAST *lhs;
  struct ExprAST *rhs;

  struct Token parent;
  struct Token identifier;

  struct ExprAST *cond;

  struct Type *sizeofArg;
};

enum TypeKind {
  VOID,
  INT,
  STRUCT,
  POINTER,
  ARRAY,
  FUNC,
  ENUM,
};

struct Type {
  enum TypeKind kind;

  // For tagged structs / enums.
  struct Token tag;

  struct Type *result;

  // For pointers / arrays, the contained type
  // For functions, linked list of arg types.
  struct Type *arg;
  struct Type *argNext;

  i32 isConst;

  // For array or integer types
  i32 size;

  i32 isSigned;

  // For funcs
  i32 isVarargs;

  // TODO: remove, now that we have cast expr.
  i32 isDecay;
};

enum DeclKind {
  VAR,
  STRUCT,
  ENUM,
  FUNC,
  ENUM_FIELD,
  IMPORT,
};

struct DeclAST {
  enum DeclKind kind;

  struct Type *type;

  struct Token name;

  // For var decl.
  struct ExprAST *init;

  // For struct decls, linked list of fields.
  // For funcs, linked list of args
  // For top level decls, linked list of decls.
  struct DeclAST *fields;
  struct DeclAST *next;

  // For function defs
  struct StmtAST *body;

  // For enum values
  i32 enumValue;

  // For function declarations that do have defs
  i32 hasDef;
};

enum StmtKind {
  DECL,
  COMPOUND,
  EXPR,

  FOR,    // for(init, cond, expr) stmt
  IF,     // if (expr) init else stmt
  WHILE,  // while(expr) stmt
  SWITCH, // switch(expr) stmt

  RETURN,
  CASE,
  BREAK,
  DEFAULT,
};

struct StmtAST {
  enum StmtKind kind;

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

// utils
i32 tokCmp(struct Token one, struct Token two) {
  if (one.kind != two.kind) {
    return 0;
  }

  i64 len1 = one.end - one.data;
  i64 len2 = two.end - two.data;
  if (len1 != len2) {
    return 0;
  }

  return memcmp(one.data, two.data, len1 as u64) == 0;
}

i32 tokCmpStr(struct Token one, const i8 *str) {
  i64 len1 = one.end - one.data;
  i64 len2 = strlen(str) as i64;
  if (len1 != len2) {
    return 0;
  }

  return memcmp(one.data, str, len1 as u64) == 0;
}

void printStr(i8 *start, i8 *end) {
  for (i8 *c = start; c != end; c++) {
    putchar(*c as i32);
  }
}

void printToken(struct Token token) {
  printf("%s", tokens[token.kind as i32]);
  printf("(");
  printStr(token.data, token.end);
  printf(") ");
}

void printType(struct Type *type) {
  if (type->isConst) {
    printf("const ");
  }
  switch (type->kind) {
  case TypeKind::INT:
    if (type->isSigned) {
      printf("i%d ", type->size);
    } else {
      printf("u%d ", type->size);
    }
    break;
  case TypeKind::VOID:
    printf("void ");
    break;
  case TypeKind::POINTER:
    printType(type->arg);
    printf("* ");
    break;
  case TypeKind::ARRAY:
    printType(type->arg);
    printf("[%d] ", type->size);
    break;
  case TypeKind::STRUCT:
    printf("struct ");
    printStr(type->tag.data, type->tag.end);
    printf(" ");
    break;
  case TypeKind::FUNC:
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
  case ExprKind::VARIABLE:
    printToken(expr->identifier);
    break;
  case ExprKind::INT:
    printf("INT(%d)", expr->value);
    break;
  case ExprKind::STR:
    printf("STR(");
    printStr(expr->identifier.data, expr->identifier.end);
    printf(")");
    break;
  case ExprKind::BINARY:
    printf("%s(", tokens[expr->op.kind as i32]);
    printExpr(expr->lhs);
    printf(" ");
    printExpr(expr->rhs);
    printf(")");
    break;
  case ExprKind::INDEX:
    printf("INDEX(");
    printExpr(expr->lhs);
    printf(" ");
    printExpr(expr->rhs);
    printf(")");
    break;
  case ExprKind::CALL:
    printf("CALL(");
    printExpr(expr->lhs);
    for (struct ExprAST *cur = expr->rhs; cur != NULL; cur = cur->rhs) {
      printf(" ");
      printExpr(cur->lhs);
    }
    printf(")");
    break;
  case ExprKind::MEMBER:
    printf("MEMBER(");
    printExpr(expr->lhs);
    printf(" %s ", tokens[expr->op.kind as i32]);
    printStr(expr->identifier.data, expr->identifier.end);
    printf(")");
    break;
  case ExprKind::UNARY:
    printf("UNARY(");
    if (expr->lhs != NULL) {
      printExpr(expr->lhs);
    }
    printf("%s", tokens[expr->op.kind as i32]);
    if (expr->rhs != NULL) {
      printExpr(expr->rhs);
    }
    printf(")");
    break;
  case ExprKind::SIZEOF:
    printf("SIZEOF(");
    if (expr->sizeofArg != NULL) {
      printType(expr->sizeofArg);
    } else {
      printExpr(expr->rhs);
    }
    printf(")");
    break;
  case ExprKind::CONDITIONAL:
    printf("COND(");
    printExpr(expr->cond);
    printf(" ? ");
    printExpr(expr->lhs);
    printf(" : ");
    printExpr(expr->rhs);
    printf(")");
    break;
  case ExprKind::ARRAY:
    printf("ARRAY(");
    for (; expr != NULL; expr = expr->rhs) {
      printExpr(expr->lhs);
      printf(", ");
    }
    printf(")");
    break;
  case ExprKind::CAST:
    printf("CAST(");
    printExpr(expr->lhs);
    printf(")");
    break;
  case ExprKind::ARG_LIST:
    break;
  case ExprKind::SCOPE:
    printf("SCOPE(");
    printToken(expr->parent);
    printf("::");
    printToken(expr->identifier);
    printf(")");
    break;
  default:
    printf("UNKOWN");
  }
}

void printDecl(struct DeclAST *decl);

void printStmt(struct StmtAST *stmt) {
  switch (stmt->kind) {
  case StmtKind::DECL:
    printDecl(stmt->decl);
    break;
  case StmtKind::COMPOUND:
    printf("{\n");
    for (struct StmtAST *cur = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      printStmt(cur);
    }
    printf("}\n");
    break;
  case StmtKind::EXPR:
    printExpr(stmt->expr);
    printf(";\n");
    break;
  case StmtKind::FOR:
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
  case StmtKind::IF:
    printf("if(");
    printExpr(stmt->expr);
    printf(")\n");
    printStmt(stmt->init);
    if (stmt->stmt != NULL) {
      printf("else\n");
      printStmt(stmt->stmt);
    }
    break;
  case StmtKind::RETURN:
    printf("return ");
    if (stmt->expr != NULL) {
      printExpr(stmt->expr);
    }
    printf(";\n");
    break;
  case StmtKind::SWITCH:
    printf("switch (");
    printExpr(stmt->expr);
    printf(")\n");
    printStmt(stmt->stmt);
    break;
  case StmtKind::DEFAULT:
    printf("default:\n");
    break;
  case StmtKind::BREAK:
    printf("break;\n");
    break;
  case StmtKind::CASE:
    printf("case ");
    printExpr(stmt->expr);
    printf(":\n");
    break;
  case StmtKind::WHILE:
    printf("while (");
    printExpr(stmt->expr);
    printf(")\n");
    printStmt(stmt->stmt);
    break;
  }
}

void printDecl(struct DeclAST *decl) {
  switch (decl->kind) {
  case DeclKind::STRUCT:
    printType(decl->type);
    printf("{\n");
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->next) {
      printf("  ");
      printDecl(field);
    }
    printf("}");
    break;
  case DeclKind::ENUM:
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
  case DeclKind::ENUM_FIELD:
    printToken(decl->name);
    break;
  case DeclKind::VAR:
    printType(decl->type);
    printToken(decl->name);

    if (decl->init != NULL) {
      printf(" = ");
      printExpr(decl->init);
    }
    break;
  case DeclKind::FUNC:
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

struct ExprAST *newExpr(enum ExprKind kind) {
  struct ExprAST *result = calloc(1, sizeof(struct ExprAST));
  result->kind = kind;
  return result;
}

struct DeclAST *newDecl() {
  struct DeclAST *decl = calloc(1, sizeof(struct DeclAST));
  return decl;
}

struct StmtAST *newStmt(enum StmtKind kind) {
  struct StmtAST *stmt = calloc(1, sizeof(struct StmtAST));
  stmt->kind = kind;
  return stmt;
}

struct Type *newType(enum TypeKind kind) {
  struct Type *type = calloc(1, sizeof(struct Type));
  type->kind = kind;
  return type;
}

struct Type *getInt32() {
  struct Type *type = newType(TypeKind::INT);
  type->isSigned = 1;
  type->size = 32;
  return type;
}

struct Type *getIPtr() {
  struct Type *type = newType(TypeKind::INT);
  type->isSigned = 1;
  type->size = 64; // TODO: target dependent
  return type;
}

struct Type *getUPtr() {
  struct Type *type = newType(TypeKind::INT);
  type->isSigned = 0;
  type->size = 64; // TODO: target dependent
  return type;
}

i32 isAssign(struct Token tok) {
  switch (tok.kind) {
  case TokenKind::EQ:
  case TokenKind::MUL_ASSIGN:
  case TokenKind::DIV_ASSIGN:
  case TokenKind::MOD_ASSIGN:
  case TokenKind::ADD_ASSIGN:
  case TokenKind::SUB_ASSIGN:
  case TokenKind::LEFT_ASSIGN:
  case TokenKind::RIGHT_ASSIGN:
  case TokenKind::AND_ASSIGN:
  case TokenKind::XOR_ASSIGN:
  case TokenKind::OR_ASSIGN:
    return 1;
  default:
    return 0;
  }
}

// Implemented in bootstrap.c needed in sema for imports.
// TODO: can parse parse the import?
struct DeclAST *parseFile(const i8 *name);
