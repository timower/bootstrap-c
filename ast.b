import libc;

enum TokenKind {
  TOK_EOF,

  // clang-format off
  // constants
  IDENTIFIER, CONSTANT, STRING_LITERAL, INT2,

  // keywords
  CONTINUE, DEFAULT, SIZEOF, STRUCT, SWITCH, RETURN, IMPORT, CONST,
  WHILE,    BREAK,   VOID,   ENUM,   CASE,   ELSE, FUNC,

  // operators
  LEFT_ASSIGN, RIGHT_ASSIGN, ELLIPSIS,  FOR, LET, SCOPE,  PTR_OP, INC_OP, DEC_OP,
  LEFT_OP, RIGHT_OP, LE_OP, GE_OP, EQ_OP, NE_OP, AND_OP, OR_OP, MUL_ASSIGN,
  DIV_ASSIGN, MOD_ASSIGN, ADD_ASSIGN, SUB_ASSIGN, AND_ASSIGN, XOR_ASSIGN,
  OR_ASSIGN, IF, AS, SEMICOLON, OPEN_BRACE, CLOSE_BRACE, COMMA, COLON, EQ,
  OPEN_PAREN, CLOSE_PAREN, OPEN_BRACKET, CLOSE_BRACKET, DOT, AND, BANG,
  TILDE, MINUS, PLUS, STAR, SLASH, PERCENT, LESS, GREATER, HAT, PIPE,
  QUESTION
  // clang-format on
};

struct Token {
  kind : TokenKind;

  data : i8 *;
  end : i8 *;
};

let tokens : const i8 *[] = {
                 "EOF",      "IDENT",   "CONST",  "STR",    "INT",

                 "continue", "default", "sizeof", "struct", "switch", "return",
                 "import",   "const",   "while",  "break",  "void",   "enum",
                 "case",     "else",    "func",   "<<=",    ">>=",    "...",
                 "for",      "let",     "::",     "->",     "++",     "--",
                 "<<",       ">>",      "<=",     ">=",     "==",     "!=",
                 "&&",       "||",      "*=",     "/=",     "%=",     "+=",
                 "-=",       "&=",      "^=",     "|=",     "if",     "as",
                 ";",        "{",       "}",      ",",      ":",      "=",
                 "(",        ")",       "[",      "]",      ".",      "&",
                 "!",        "~",       "-",      "+",      "*",      "/",
                 "%",        "<",       ">",      "^",      "|",      "?",
};

enum TypeKind {
  VOID,
  INT,
  STRUCT,
  POINTER,
  ARRAY,
  FUNC,
  ENUM,

  TAG, // For tagged structs / enums resolved in sema.
};

struct Type {
  kind : TypeKind;

  // For tagged structs / enums.
  tag : Token;

  result : Type *;

  // For pointers / arrays, the contained type
  // For functions, linked list of arg types.
  arg : Type *;
  argNext : Type *;

  isConst : i32;

  // For array or integer types
  size : i32;

  isSigned : i32;

  // For funcs
  isVarargs : i32;
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
  kind : ExprKind;

  type : Type *;

  // primary_expr
  // \{
  // int
  value : i32;
  // \}

  // binary
  op : Token;
  lhs : ExprAST *;
  rhs : ExprAST *;

  parent : Token;
  identifier : Token;

  cond : ExprAST *;

  sizeofArg : Type *;
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
  kind : DeclKind;

  type : Type *;

  name : Token;

  // For var decl.
  init : ExprAST *;

  // For  decls, linked list of fields.
  // For funcs, linked list of args
  // For top level decls, linked list of decls.
  fields : DeclAST *;
  next : DeclAST *;

  // For function defs
  body : StmtAST *;

  // For enum values
  enumValue : i32;

  // For function declarations that do have defs
  hasDef : i32;
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
  kind : StmtKind;

  decl : DeclAST *;

  // For for
  init : StmtAST *;
  cond : StmtAST *;

  // For expr stmts
  expr : ExprAST *;

  // For compound stmts
  stmt : StmtAST *;

  // To form linked list of compound stmts
  nextStmt : StmtAST *;
};

// utils
func tokCmp(one : Token, two : Token) -> i32 {
  if (one.kind != two.kind) {
    return 0;
  }

  let len1 = one.end - one.data;
  let len2 = two.end - two.data;
  if (len1 != len2) {
    return 0;
  }

  return memcmp(one.data, two.data, len1 as u64) == 0;
}

func tokCmpStr(one : Token, str : const i8 *) -> i32 {
  let len1 = one.end - one.data;
  let len2 = strlen(str) as i64;
  if (len1 != len2) {
    return 0;
  }

  return memcmp(one.data, str, len1 as u64) == 0;
}

func printStr(start : i8 *, end : i8 *) {
  for (let c = start; c != end; c++) {
    putchar(*c as i32);
  }
}

func printToken(token : Token) {
  if (token.kind != TokenKind::IDENTIFIER) {
    printf("%s", tokens[token.kind as i32]);
  }
  printStr(token.data, token.end);
}

func printType(type : Type *) {
  if (type == NULL) {
    printf("nullType ");
    return;
  }

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
    if (type->size < 0) {
      printf("[] ");
    } else {
      printf("[%d] ", type->size);
    }
    break;
  case TypeKind::STRUCT:
    printf("struct ");
    printStr(type->tag.data, type->tag.end);
    printf(" ");
    break;
  case TypeKind::FUNC:
    printf("(");
    for (let arg : Type * = type->arg; arg != NULL; arg = arg->argNext) {
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
  case TypeKind::ENUM:
    printf("enum ");
    printStr(type->tag.data, type->tag.end);
    printf(" ");
    break;
  case TypeKind::TAG:
    printf("tag ");
    printStr(type->tag.data, type->tag.end);
    printf(" ");
    break;
  }
}

func printExpr(expr : ExprAST *) {
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
    for (let cur : ExprAST * = expr->rhs; cur != NULL; cur = cur->rhs) {
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

func printDecl(decl : DeclAST *);

func printStmt(stmt : StmtAST *) {
  switch (stmt->kind) {
  case StmtKind::DECL:
    printDecl(stmt->decl);
    break;
  case StmtKind::COMPOUND:
    printf("{\n");
    for (let cur : StmtAST * = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
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
    printf(") {\n");
    for (let cur : StmtAST * = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      printStmt(cur);
    }
    printf("}\n");
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
    for (let cur : StmtAST * = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      printStmt(cur);
    }
    break;
  case StmtKind::WHILE:
    printf("while (");
    printExpr(stmt->expr);
    printf(")\n");
    printStmt(stmt->stmt);
    break;
  }
}

func printDecl(decl : DeclAST *) {
  switch (decl->kind) {
  case DeclKind::STRUCT:
    printType(decl->type);
    printf("{\n");
    for (let field : DeclAST * = decl->fields; field != NULL;
         field = field->next) {
      printf("  ");
      printDecl(field);
    }
    printf("}");
    break;
  case DeclKind::ENUM:
    printType(decl->type);
    printf("{\n");
    for (let field : DeclAST * = decl->fields; field != NULL;
         field = field->next) {
      printf("  ");
      printDecl(field);
    }
    printf("} ");
    break;
  case DeclKind::ENUM_FIELD:
    printToken(decl->name);
    break;
  case DeclKind::VAR:
    printf("let ");
    printToken(decl->name);
    printf(" : ");
    printType(decl->type);

    if (decl->init != NULL) {
      printf(" = ");
      printExpr(decl->init);
    }
    break;
  case DeclKind::FUNC:
    printf("func ");
    printToken(decl->name);
    printf("(");
    for (let field : DeclAST * = decl->fields; field != NULL;
         field = field->next) {
      printf("  ");
      printDecl(field);
      if (field->next != NULL) {
        printf(",");
      }
    }
    printf(") -> ");
    printType(decl->type);

    if (decl->body != NULL) {
      printStmt(decl->body);
    }
    break;
  case DeclKind::IMPORT:
    printf("Import ");
    printToken(decl->name);
    printf("\n");
    break;
  }
  printf("\n");
}

func newExpr(kind : ExprKind) -> ExprAST * {
  let result : ExprAST * = calloc(1, sizeof(struct ExprAST));
  result->kind = kind;
  return result;
}

func newDecl() -> DeclAST * {
  let decl : DeclAST * = calloc(1, sizeof(struct DeclAST));
  return decl;
}

func newStmt(kind : StmtKind) -> StmtAST * {
  let stmt : StmtAST * = calloc(1, sizeof(struct StmtAST));
  stmt->kind = kind;
  return stmt;
}

func newType(kind : TypeKind) -> Type * {
  let type : Type * = calloc(1, sizeof(struct Type));
  type->kind = kind;
  return type;
}

func getCharType() -> Type * {
  let type : Type * = newType(TypeKind::INT);
  type->isSigned = 1;
  type->size = 8;
  return type;
}

func getInt32() -> Type * {
  let type : Type * = newType(TypeKind::INT);
  type->isSigned = 1;
  type->size = 32;
  return type;
}

func getIPtr() -> Type * {
  let type : Type * = newType(TypeKind::INT);
  type->isSigned = 1;
  type->size = 64; // TODO: target dependent
  return type;
}

func getUPtr() -> Type * {
  let type : Type * = newType(TypeKind::INT);
  type->isSigned = 0;
  type->size = 64; // TODO: target dependent
  return type;
}

func isAssign(tok : Token) -> i32 {
  switch (tok.kind) {
    // clang-format off
  case TokenKind::EQ,
       TokenKind::MUL_ASSIGN,
       TokenKind::DIV_ASSIGN,
       TokenKind::MOD_ASSIGN,
       TokenKind::ADD_ASSIGN,
       TokenKind::SUB_ASSIGN,
       TokenKind::LEFT_ASSIGN,
       TokenKind::RIGHT_ASSIGN,
       TokenKind::AND_ASSIGN,
       TokenKind::XOR_ASSIGN,
       TokenKind::OR_ASSIGN:
    return 1;
    // clang-format on
  default:
    return 0;
  }
}

// Implemented in bootstrap.c needed in sema for imports.
// TODO: can parse parse the import?
func parseFile(name : const i8 *) -> DeclAST *;
