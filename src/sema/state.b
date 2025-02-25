import ast;
import ast.print;

struct ImportList {
  name: i8*;
  next: ImportList*;
};

struct SemaState {
  parent: SemaState*;

  // Return type of the current function.
  result: Type*;

  // Local variables and enum fields.
  locals: DeclList*;

  // struct tag list.
  types: DeclList*;

  // Extra decls added during sema, used for string literals.
  // Should only be added to the root sema state.
  extraDecls: DeclAST*;
  strCount: i32;

  imports: ImportList*;
  target: i8*;
};

func newState(parent: SemaState*) -> SemaState {
  let state = SemaState {
    parent = parent,
    result = parent->result,
  };
  return state;
}


// 2. sema
func failSema(loc: SourceLoc, msg: const i8*) {
  dprintf(STDERR, "%s:%d:%d: sema error: %s\n", loc.fileName, loc.line, loc.column, msg);
  exit(1);
}

func failSemaExpr(expr: ExprAST*, msg: const i8*) {
  printExpr(expr);
  printf("\n");
  failSema(expr->location, msg);
}

func failSemaDecl(decl: DeclAST*, msg: const i8*) {
  printDecl(decl);
  printf("\n");
  failSema(decl->location, msg);
}

func failSemaStmt(stmt: StmtAST*, msg: const i8*) {
  printStmt(stmt);
  printf("\n");
  failSema(stmt->location, msg);
}

func getRoot(state: SemaState*) -> SemaState* {
  while (state->parent != null) {
    state = state->parent;
  }
  return state;
}

func getNullDecl(name: i8*) -> DeclAST* {
  let nullTok = Token {};
  nullTok.kind = TokenKind::IDENTIFIER;
  nullTok.data = name;
  nullTok.end = name + strlen(name);

  // Add null as a nullptr
  let nullDecl = newDecl(DeclKind::ENUM_FIELD);
  nullDecl->name = nullTok;
  nullDecl->enumValue = 0;
  nullDecl->type = newType(TypeKind::Pointer {
    pointee = newType(TypeKind::Void {}),
  });

  return nullDecl;
}

func initSemaState(target: i8*) -> SemaState {
  let nullDecl = getNullDecl("null");
  return SemaState {
    target = target,
    locals = newDeclList(nullDecl),
  };
}
