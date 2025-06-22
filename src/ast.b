import libc;
import ast.token;

struct SourceLoc {
  line: i32;
  column: i32;
  fileName: i8*;
  // TODO: if needed:
  // ptr: i8*;
};

struct Comment {
  location: SourceLoc;
  value: Token;
  next: Comment*;
};

union TypeKind {
  Void {}
  Bool {}
  Int {
    size: i32;
    isSigned: bool;
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
    isVarargs: bool;
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
  isConst: bool;
  // TODO: Source loc
};


struct FieldIndex {
  fieldName: Token;
  value: ExprAST*;

  // Set during sema
  index: i32;

  next: FieldIndex*;
};

union ExprKind {
  Int {
    value: i32;
    token: Token;
  }
  Str {
    identifier: Token;
  }
  Variable {
    identifier: Token;
  }
  Array {
    elements: ExprAST*;
  }
  Struct {
    identifier: Token;
    parent: Token;
    fieldIndices: FieldIndex*;
  }
  Call {
    function: ExprAST*;
    args: ExprAST*;
  }
  Index {
    array: ExprAST*;
    index: ExprAST*;
  }
  Member {
    object: ExprAST*;
    identifier: Token;
    op: Token;
    fieldIndex: i32;
  }
  Scope {
    parent: Token;
    identifier: Token;
    enumValue: i32;
  }
  Unary {
    op: Token;
    postfix: ExprAST*;
    prefix: ExprAST*;
  }
  Sizeof {
    expr: ExprAST*;
    typeArg: Type*;
    value: i32;
  }
  Conditional {
    cond: ExprAST*;
    trueExpr: ExprAST*;
    falseExpr: ExprAST*;
  }
  Binary {
    op: Token;
    lhs: ExprAST*;
    rhs: ExprAST*;
  }
  Cast {
    expr: ExprAST*;
    castKind: CastKind;
    fieldIndex: i32;
  }
  Paren {
    expr: ExprAST*;
  }
  Let {
    decl: DeclAST*;
  }
};

enum CastKind {
  Noop,

  StructUnion,
  UnionStructPtr,

  PtrToInt,

  // Int casts
  Trunc,
  Sext,
  Zext,
};


// Represents an expression in the AST.
struct ExprAST {
  kind: ExprKind;
  type: Type*;
  location: SourceLoc;
  next: ExprAST*;
};

union DeclKind {
  Var {
    init: ExprAST*;
    isExtern: bool;
  }
  Const {
    init: ExprAST*;
    enumValue: i32;
  }
  Struct {
    fields: DeclAST*;
  }
  Enum {
    fields: DeclAST*;
  }
  Func {
    fields: DeclAST*;
    body: StmtAST*;
    isExtern: bool;
  }
  Import {
    path: ExprAST*;
  }
  Union {
    // TODO: Doesn't need to be declList? Can be intrinsic.
    subTypes: DeclList*;
    maxSize: i32;
  }
  EnumField {
    enumValue: i32;
  }
};

struct DeclAST {
  kind: DeclKind;

  type: Type*;
  name: Token;

  // To form linked list of declarations
  next: DeclAST*;

  location: SourceLoc;
  endLocation: SourceLoc;

  // Only for concrete parsing.
  comments: Comment*;
};

struct DeclList {
  decl: DeclAST*;

  next: DeclList*;
};

union StmtKind {
  Compound {
    stmt: StmtAST*;
  }
  Expr {
    expr: ExprAST*;
  }
  For {
    init: StmtAST*;
    cond: StmtAST*;
    update: ExprAST*;
    body: StmtAST*;
  }
  If {
    cond: ExprAST*;
    thenStmt: StmtAST*;
    elseStmt: StmtAST*;
  }
  While {
    cond: ExprAST*;
    body: StmtAST*;
  }
  Switch {
    expr: ExprAST*;
    body: StmtAST*;
  }
  Return {
    expr: ExprAST*;
  }
  Case {
    expr: ExprAST*;
    body: StmtAST*;
  }
  Break {}
  Default {
    body: StmtAST*;
  }
};

struct StmtAST {
  kind: StmtKind;

  // To form linked list of statements
  next: StmtAST*;

  location: SourceLoc;
  endLocation: SourceLoc;
  comments: Comment*;
};


// utils
func tokCmp(one: Token, two: Token) -> bool {
  if (one.kind != two.kind) {
    return false;
  }

  let len1 = one.end - one.data;
  let len2 = two.end - two.data;
  if (len1 != len2) {
    return false;
  }

  return memcmp(one.data, two.data, len1 as u64) == 0;
}

func tokCmpStr(one: Token, str: const i8*) -> bool {
  let len1 = one.end - one.data;
  let len2 = strlen(str) as i64;
  if (len1 != len2) {
    return false;
  }

  return memcmp(one.data, str, len1 as u64) == 0;
}

func newExpr(kind: ExprKind) -> ExprAST* {
  let result: ExprAST* = calloc(1, sizeof(struct ExprAST));
  result->kind = kind;
  return result;
}

func newFieldIndex(name: Token, field: ExprAST*) -> FieldIndex* {
  let result: FieldIndex* = calloc(1, sizeof(struct FieldIndex));
  result->fieldName = name;
  result->value = field;
  result->index = -1;
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
    isSigned = true,
  });
}

func getInt32() -> Type* {
  return newType(TypeKind::Int {
    size = 32,
    isSigned = true,
  });
}

func getBool() -> Type* {
  return newType(TypeKind::Bool {});
}

func getIPtr() -> Type* {
  // TODO: target dependent
  return newType(TypeKind::Int {
    size = 64,
    isSigned = true,
  });
}

func getUPtr() -> Type* {
  // TODO: target dependent
  return newType(TypeKind::Int {
    size = 64,
    isSigned = false,
  });
}

func isAssign(tok: Token) -> bool {
  switch (tok.kind) {
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
      return true;

    default:
      return false;
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
    case ExprKind::Binary as binary:
      if (isAssign(binary.op)) {
        return 5;
      }
      if (binary.op.kind == TokenKind::COMMA) {
        return 1;
      }
      return getBinOpPrecedence(binary.op);

    case ExprKind::Unary as unary:
      // Unary postfix
      if (unary.prefix == null) {
        return 120;
      }
      return 110;

    case ExprKind::Call,
         ExprKind::Index,
         ExprKind::Member,
         ExprKind::Struct,
         ExprKind::Array:
      return 120;
    case ExprKind::Cast,
         ExprKind::Sizeof:
      return 110;
    case ExprKind::Conditional:
      return 9;

    case ExprKind::Int,
         ExprKind::Str,
         ExprKind::Variable,
         ExprKind::Scope,
         ExprKind::Paren,
         ExprKind::Let:
      return 200;
  }
}
