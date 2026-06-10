import stdlib.libc;

import alloc;
import ast.token;

import target;

struct Comment {
  location: SourceLoc*;
  value: Token;
  next: Comment*;
}

union TypeKind {
  Void {}
  Bool {}
  Int {
    size: i32;
    isSigned: bool;
    isPtr: bool;
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
    typeArgs: Type*;
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
  Typeof {
    expr: ExprAST*;
  }
  Slice {
    element: Type*;
  }
}

struct Type {
  kind: TypeKind;

  // Intrinsic list for function arguments.
  next: Type*;
  isConst: bool;

  location: SourceLoc*;
}


struct FieldIndex {
  fieldName: Token;
  value: ExprAST*;

  // Set during sema
  index: i32;

  next: FieldIndex*;
}

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
  GenericInstantiation {
    function: Token;
    typeArgs: Type*;

    // Filled during sema, name of monomorphized instance.
    instance: Token;
  }
  Index {
    array: ExprAST*;
    index: ExprAST*;
  }
  SliceIndex {
    slice: ExprAST*;
    start: ExprAST*;
    end: ExprAST*;
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
}

enum CastKind {
  Noop,

  StructUnion,
  UnionStructPtr,

  // Int casts
  Trunc,
  Sext,
  Zext,
}


// Represents an expression in the AST.
struct ExprAST {
  kind: ExprKind;
  type: Type*;
  location: SourceLoc*;
  next: ExprAST*;
}

union DeclKind {
  Var {
    isExtern: bool;
    init: ExprAST*;
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
    args: DeclAST*;
    body: StmtAST*;
    isExtern: bool;

    // Optional name, only set for member functions.
    structName: Token;
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
}

struct DeclAST {
  kind: DeclKind;

  type: Type*;
  name: Token;

  // To form linked list of declarations
  next: DeclAST*;

  location: SourceLoc*;
  endLocation: SourceLoc*;

  // Only for concrete parsing.
  comments: Comment*;
}

struct DeclList {
  decl: DeclAST*;

  next: DeclList*;
}

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
  Continue {}
  Default {
    body: StmtAST*;
  }
  Defer {
    stmt: StmtAST*;
  }
}

struct StmtAST {
  kind: StmtKind;

  // To form linked list of statements
  next: StmtAST*;

  location: SourceLoc*;
  endLocation: SourceLoc*;
  comments: Comment*;
}


// utils
func newExpr(allocator: Allocator*, kind: ExprKind) -> ExprAST* {
  let result = alloc(allocator, sizeof(struct ExprAST)) as ExprAST*;
  result->kind = kind;
  return result;
}

func newFieldIndex(allocator: Allocator*, name: Token, field: ExprAST*) -> FieldIndex* {
  let result = alloc(allocator, sizeof(struct FieldIndex)) as FieldIndex*;
  result->fieldName = name;
  result->value = field;
  result->index = -1;
  return result;
}

func newDecl(allocator: Allocator*, kind: DeclKind) -> DeclAST* {
  let decl = alloc(allocator, sizeof(struct DeclAST)) as DeclAST*;
  decl->kind = kind;
  return decl;
}

func newDeclList(allocator: Allocator*, decl: DeclAST*) -> DeclList* {
  let res = alloc(allocator, sizeof(struct DeclList)) as DeclList*;
  res->decl = decl;
  return res;
}

func newStmt(allocator: Allocator*, kind: StmtKind) -> StmtAST* {
  let stmt = alloc(allocator, sizeof(struct StmtAST)) as StmtAST*;
  stmt->kind = kind;
  return stmt;
}


func newType(allocator: Allocator*, kind: TypeKind) -> Type* {
  let type = alloc(allocator, sizeof(struct Type)) as Type*;
  type->kind = kind;
  return type;
}

func newComment(allocator: Allocator*, token: Token) -> Comment* {
  let comment = alloc(allocator, sizeof(struct Comment)) as Comment*;
  comment->value = token;
  comment->location = token.location;
  return comment;
}

func getCharType(allocator: Allocator*) -> Type* {
  return newType(allocator, TypeKind::Int {
    size = 8,
    isSigned = true,
  });
}

func getInt32(allocator: Allocator*) -> Type* {
  return newType(allocator, TypeKind::Int {
    size = 32,
    isSigned = true,
  });
}

func getBool(allocator: Allocator*) -> Type* {
  return newType(allocator, TypeKind::Bool {});
}

func getIPtr(allocator: Allocator*, target: Target*) -> Type* {
  return newType(allocator, TypeKind::Int {
    size = getIntSize(target),
    isSigned = true,
    isPtr = true,
  });
}

func getUPtr(allocator: Allocator*, target: Target*) -> Type* {
  return newType(allocator, TypeKind::Int {
    size = getIntSize(target),
    isSigned = false,
    isPtr = true,
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


func isGeneric(decl: DeclAST*) -> bool {
  if (let funcKind = decl->kind as DeclKind::Func*) {
    let funcType = decl->type->kind as TypeKind::Func*;
    if (funcType == null) {
      unreachable("Func decl without func type");
    }
    if (funcType->typeArgs != null) {
      return true;
    }
  }
  return false;
}

func getFunctionType(callExpr: ExprKind::Call*) -> TypeKind::Func* {
  let ptrType = callExpr->function->type->kind as TypeKind::Pointer*;
  if (ptrType != null) {
    return ptrType->pointee->kind as TypeKind::Func*;
  }
  return callExpr->function->type->kind as TypeKind::Func*;
}
