import libc;

struct SourceLoc {
  line: i32;
  column: i32;
  fileName: i8*;
  // TODO: if needed:
  // ptr: i8*;
};

enum TokenKind {
  TOK_EOF,

  // clang-format off
  // constants
  IDENTIFIER,
  CONSTANT,
  STRING_LITERAL,
  INT2,
  COMMENT,

  // keywords
  CONTINUE,
  DEFAULT,
  SIZEOF,
  STRUCT,
  SWITCH,
  RETURN,
  IMPORT,
  CONST,
  WHILE,
  BREAK,
  UNION,
  VOID,
  ENUM,
  CASE,
  ELSE,
  FUNC,

  // operators
  LEFT_ASSIGN,
  RIGHT_ASSIGN,
  ELLIPSIS,
  FOR,
  LET,
  SCOPE,
  PTR_OP,
  INC_OP,
  DEC_OP,
  LEFT_OP,
  RIGHT_OP,
  LE_OP,
  GE_OP,
  EQ_OP,
  NE_OP,
  AND_OP,
  OR_OP,
  MUL_ASSIGN,
  DIV_ASSIGN,
  MOD_ASSIGN,
  ADD_ASSIGN,
  SUB_ASSIGN,
  AND_ASSIGN,
  XOR_ASSIGN,
  OR_ASSIGN,
  IF,
  AS,
  SEMICOLON,
  OPEN_BRACE,
  CLOSE_BRACE,
  COMMA,
  COLON,
  EQ,
  OPEN_PAREN,
  CLOSE_PAREN,
  OPEN_BRACKET,
  CLOSE_BRACKET,
  DOT,
  AND,
  BANG,
  TILDE,
  MINUS,
  PLUS,
  STAR,
  SLASH,
  PERCENT,
  LESS,
  GREATER,
  HAT,
  PIPE,
  QUESTION,  // clang-format on
};

struct Token {
  kind: TokenKind;

  data: i8*;
  end: i8*;
};

struct Comment {
  location: SourceLoc;
  value: Token;
  next: Comment*;
};

let tokens: const i8*[] = {
  "EOF", "IDENT", "CONST", "STR", "INT", "COMMENT",
  "continue", "default", "sizeof", "struct", "switch", "return",
  "import", "const", "while", "break", "union", "void", "enum",
  "case", "else", "func", "<<=", ">>=", "...",
  "for", "let", "::", "->", "++", "--",
  "<<", ">>", "<=", ">=", "==", "!=",
  "&&", "||", "*=", "/=", "%=", "+=",
  "-=", "&=", "^=", "|=", "if", "as",
  ";", "{", "}", ",", ":", "=",
  "(", ")", "[", "]", ".", "&",
  "!", "~", "-", "+", "*", "/",
  "%", "<", ">", "^", "|", "?",
};

union TypeKind {
  Void {
  }
  Int {
    size: i32;
    isSigned: i32;
  }
  Enum {
    tag: Token;
  }
  Pointer {
    pointee: Type*;
  }
  Array {
    element: Type*;
    size: i32;
  }
  Func {
    result: Type*;
    args: Type*;
    isVarargs: i32;
  }
  Struct {
    tag: Token;
    parent: Type*;
  }
  Union {
    tag: Token;
  }
  Tag {
    tag: Token;
    parent: Token;    // optional, EOF if not used.
  }
};

struct Type {
  kind: TypeKind;

  // Intrinsic list for function arguments.
  next: Type*;
  isConst: i32;
  // TODO: Source loc
};


enum ExprKind {
  INT,  // value
  STR,  // "identifier"
  VARIABLE,  // identifier

  ARRAY,  // {a, b, c}
  STRUCT,  // identifier { a, b, c }

  CALL,  // lhs(rhs->lhs, rhs->rhs->lhs, ..)
  INDEX,  // lhs[rhs]

  MEMBER,  // lhs.identifier, lhs->identifier,
  SCOPE,  // parent::identifier

  UNARY,  // lhs++, lhs-- or --rhs ++rhs based on op
  SIZEOF,  // sizeof(rhs) or sizeof(sizeofArg)

  CONDITIONAL,  // cond ? lhs : rhs

  ARG_LIST,  // lhs, rhs

  BINARY,  // a + b, ...

  CAST,  // lhs as type

  PAREN,  // (lhs)
};

enum CastKind {
  Noop,

  StructUnion,
  UnionStructPtr,

  // Int casts
  Trunc,
  Sext,
  Zext,
};


// Represents an expression in the AST.
struct ExprAST {
  kind: ExprKind;

  type: Type*;

  // primary_expr
  // \{
  // int
  value: i32;

  // \}
  // binary
  op: Token;
  lhs: ExprAST*;
  rhs: ExprAST*;

  parent: Token;
  identifier: Token;

  cond: ExprAST*;

  sizeofArg: Type*;

  castKind: CastKind;

  location: SourceLoc;
};

enum DeclKind {
  // Local & Global decls
  VAR,

  // Global decls
  STRUCT,
  ENUM,
  FUNC,
  IMPORT,
  UNION,

  // Specials
  ENUM_FIELD,
};

struct DeclAST {
  kind: DeclKind;

  type: Type*;

  name: Token;

  // For var decl.
  init: ExprAST*;

  // For  decls, linked list of fields.
  // For funcs, linked list of args
  // For top level decls, linked list of decls.
  fields: DeclAST*;
  next: DeclAST*;

  // For function defs
  body: StmtAST*;

  // For enum values
  enumValue: i32;

  // For function declarations that do have defs
  hasDef: i32;

  location: SourceLoc;
  endLocation: SourceLoc;

  // Only for concrete parsing.
  comments: Comment*;

  subTypes: DeclList*;
};

struct DeclList {
  decl: DeclAST*;

  next: DeclList*;
};

enum StmtKind {
  DECL,
  COMPOUND,
  EXPR,

  FOR,  // for(init, cond, expr) stmt
  IF,  // if (expr) init else stmt
  WHILE,  // while(expr) stmt
  SWITCH,  // switch(expr) stmt

  RETURN,
  CASE,
  BREAK,
  DEFAULT,
};

struct StmtAST {
  kind: StmtKind;

  decl: DeclAST*;

  // For for
  init: StmtAST*;
  cond: StmtAST*;

  // For expr stmts
  expr: ExprAST*;

  // For compound stmts
  stmt: StmtAST*;

  // To form linked list of compound stmts
  nextStmt: StmtAST*;

  location: SourceLoc;
  endLocation: SourceLoc;
  comments: Comment*;
};


// utils
func tokCmp(one: Token, two: Token) -> i32 {
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

func tokCmpStr(one: Token, str: const i8*) -> i32 {
  let len1 = one.end - one.data;
  let len2 = strlen(str) as i64;
  if (len1 != len2) {
    return 0;
  }

  return memcmp(one.data, str, len1 as u64) == 0;
}

func newExpr(kind: ExprKind) -> ExprAST* {
  let result: ExprAST* = calloc(1, sizeof(struct ExprAST));
  result->kind = kind;
  return result;
}

func newDecl(kind: DeclKind) -> DeclAST* {
  let decl: DeclAST* = calloc(1, sizeof(struct DeclAST));
  decl->kind = kind;
  return decl;
}

func newDeclList(decl: DeclAST*) -> DeclList* {
  let res: DeclList* = calloc(1, sizeof(struct DeclList));
  res->decl = decl;
  return res;
}

func newStmt(kind: StmtKind) -> StmtAST* {
  let stmt: StmtAST* = calloc(1, sizeof(struct StmtAST));
  stmt->kind = kind;
  return stmt;
}


func newType(kind: TypeKind) -> Type* {
  let type: Type* = calloc(1, sizeof(struct Type));
  type->kind = kind;
  return type;
}

func newComment(token: Token) -> Comment* {
  let comment: Comment* = calloc(1, sizeof(struct Comment));
  comment->value = token;
  return comment;
}

func getCharType() -> Type* {
  return newType(TypeKind::Int {
    size = 8,
    isSigned = 1,
  });
}

func getInt32() -> Type* {
  return newType(TypeKind::Int {
    size = 32,
    isSigned = 1,
  });
}

func getIPtr() -> Type* {
  // TODO: target dependent
  return newType(TypeKind::Int {
    size = 64,
    isSigned = 1,
  });
}

func getUPtr() -> Type* {
  // TODO: target dependent
  return newType(TypeKind::Int {
    size = 64,
    isSigned = 0,
  });
}

func isAssign(tok: Token) -> i32 {
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

func getBinOpPrecedence(tok: Token) -> i32 {
  switch (tok.kind) {
    case TokenKind::STAR, TokenKind::SLASH, TokenKind::PERCENT:
      return 100;

    case TokenKind::PLUS, TokenKind::MINUS:
      return 90;

    case TokenKind::LEFT_OP, TokenKind::RIGHT_OP:
      return 80;

    case TokenKind::LESS, TokenKind::GREATER, TokenKind::LE_OP, TokenKind::GE_OP:
      return 70;

    case TokenKind::EQ_OP, TokenKind::NE_OP:
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

func getExprPrecedence(expr: ExprAST*) -> i32 {
  switch (expr->kind) {
    case ExprKind::BINARY:
      if (isAssign(expr->op)) {
        return 5;
      }
      if (expr->op.kind == TokenKind::COMMA) {
        return 1;
      }
      return getBinOpPrecedence(expr->op);

    case ExprKind::UNARY:
      // Unary postfix
      if (expr->rhs == null) {
        return 120;
      }
      return 110;

    case ExprKind::CALL, ExprKind::INDEX, ExprKind::MEMBER, ExprKind::STRUCT,
         ExprKind::ARRAY:
      return 120;
    case ExprKind::CAST, ExprKind::SIZEOF:
      return 110;
    case ExprKind::CONDITIONAL:
      return 9;

    case ExprKind::INT, ExprKind::STR, ExprKind::VARIABLE, ExprKind::SCOPE,
         ExprKind::ARG_LIST, ExprKind::PAREN:
      return 200;
  }
}
