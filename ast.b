import libc;

struct Token {
  enum {
    TOK_EOF,

    // clang-format off
    // constants
    IDENTIFIER, CONSTANT, STRING_LITERAL, INT2,

    // keywords
    CONTINUE, DEFAULT, SIZEOF, STRUCT, SWITCH, RETURN, IMPORT, CONST,
    WHILE,    BREAK,   VOID,   ENUM,   CASE,   ELSE,

    // operators
    LEFT_ASSIGN, RIGHT_ASSIGN, ELLIPSIS,  FOR, PTR_OP, INC_OP, DEC_OP, LEFT_OP,
    RIGHT_OP, LE_OP, GE_OP, EQ_OP, NE_OP, AND_OP, OR_OP, MUL_ASSIGN,
    DIV_ASSIGN, MOD_ASSIGN, ADD_ASSIGN, SUB_ASSIGN, AND_ASSIGN, XOR_ASSIGN,
    OR_ASSIGN, IF, AS, SEMICOLON, OPEN_BRACE, CLOSE_BRACE, COMMA, COLON, EQ,
    OPEN_PAREN, CLOSE_PAREN, OPEN_BRACKET, CLOSE_BRACKET, DOT, AND, BANG,
    TILDE, MINUS, PLUS, STAR, SLASH, PERCENT, LESS, GREATER, HAT, PIPE,
    QUESTION,
    // clang-format on
  } kind;

  i8 *data;
  i8 *end;
};

const i8 *tokens[] = {
    "EOF",      "IDENT",   "CONST",  "STR",    "INT",

    "continue", "default", "sizeof", "struct", "switch", "return", "import",
    "const",    "while",   "break",  "void",   "enum",   "case",   "else",
    "<<=",      ">>=",     "...",    "for",    "->",     "++",     "--",
    "<<",       ">>",      "<=",     ">=",     "==",     "!=",     "&&",
    "||",       "*=",      "/=",     "%=",     "+=",     "-=",     "&=",
    "^=",       "|=",      "if",     "as",     ";",      "{",      "}",
    ",",        ":",       "=",      "(",      ")",      "[",      "]",
    ".",        "&",       "!",      "~",      "-",      "+",      "*",
    "/",        "%",       "<",      ">",      "^",      "|",      "?",
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
  i32 value;
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
    INT_TYPE2,
    STRUCT_TYPE,
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

  i32 isConst;

  // For array or integer types
  i32 size;

  i32 isSigned;

  // For funcs
  i32 isVarargs;

  // TODO: remove, now that we have cast expr.
  i32 isDecay;
};

struct DeclAST {
  enum {
    VAR_DECL,
    STRUCT_DECL,
    ENUM_DECL,
    FUNC_DECL,
    ENUM_FIELD_DECL,
    IMPORT_DECL,
  } kind;

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
  printf("%s", tokens[token.kind]);
  printf("(");
  printStr(token.data, token.end);
  printf(") ");
}


void printType(struct Type *type) {
  if (type->isConst) {
    printf("const ");
  }
  switch (type->kind) {
  case INT_TYPE2:
    if (type->isSigned) {
      printf("i%d ", type->size);
    } else {
      printf("u%d ", type->size);
    }
    break;
  case VOID_TYPE:
    printf("void ");
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
    printf("%s(", tokens[expr->op.kind]);
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
    printf(" %s ", tokens[expr->op.kind]);
    printStr(expr->identifier.data, expr->identifier.end);
    printf(")");
    break;
  case UNARY_EXPR:
    printf("UNARY(");
    if (expr->lhs != NULL) {
      printExpr(expr->lhs);
    }
    printf("%s", tokens[expr->op.kind]);
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
    if (stmt->stmt != NULL) {
      printf("else\n");
      printStmt(stmt->stmt);
    }
    break;
  case RETURN_STMT:
    printf("return ");
    if (stmt->expr != NULL) {
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
  case ENUM_FIELD_DECL:
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

struct ExprAST *newExpr(i32 kind) {
  struct ExprAST *result = calloc(1, sizeof(struct ExprAST));
  result->kind = kind;
  return result;
}

struct DeclAST *newDecl() {
  struct DeclAST *decl = calloc(1, sizeof(struct DeclAST));
  return decl;
}

struct Type *newType(i32 kind) {
  struct Type *type = calloc(1, sizeof(struct Type));
  type->kind = kind;
  return type;
}

struct Type *getInt32() {
  struct Type *type = newType(INT_TYPE2);
  type->isSigned = 1;
  type->size = 32;
  return type;
}

struct Type *getIPtr() {
  struct Type *type = newType(INT_TYPE2);
  type->isSigned = 1;
  type->size = 64; // TODO: target dependent
  return type;
}

struct Type *getUPtr() {
  struct Type *type = newType(INT_TYPE2);
  type->isSigned = 0;
  type->size = 64; // TODO: target dependent
  return type;
}

i32 isAssign(struct Token tok) {
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

// Implemented in bootstrap.c needed in sema for imports.
// TODO: can parse parse the import?
struct DeclAST *parseFile(const i8 *name);
