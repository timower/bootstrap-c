import ast;
import ast.print;

struct ImportList {
  name: i8*;
  next: ImportList*;
}

struct PathCache {
  importName: i8*;
  resolvedPath: i8*;
  next: PathCache*;
}

struct SemaState {
  target: i8*;

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

  // Used to give each string a unique name.
  strCount: i32;

  // List of files imported.
  imports: ImportList*;

  // Cache of loaded paths to reduce realpath use.
  pathCache: PathCache*;

  // Set to true to give LSP related output during sema.
  semaLspMode: bool;
}

func newState(parent: SemaState*) -> SemaState {
  let state = SemaState {
    parent = parent,
    result = parent->result,
    target = parent->target,
    semaLspMode = parent->semaLspMode,
  };
  return state;
}


// 2. sema
func printLoc(loc: SourceLoc) {
  fprintf(getStderr(), "%s:%d:%d: ", loc.fileName, loc.line, loc.column);
}

func failSema(loc: SourceLoc, msg: const i8*) {
  printLoc(loc);
  fprintf(getStderr(), "sema error: %s\n", msg);
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
  let nullDecl = newDecl(DeclKind::EnumField {});
  nullDecl->name = nullTok;
  (&nullDecl->kind as DeclKind::EnumField*)->enumValue = 0;
  nullDecl->type = newType(TypeKind::Pointer {
    pointee = newType(TypeKind::Void {}),
  });

  return nullDecl;
}

func initSemaState(target: i8*, lspMode: bool) -> SemaState {
  let nullDecl = getNullDecl("null");
  return SemaState {
    target = target,
    locals = newDeclList(nullDecl),
    semaLspMode = lspMode,
  };
}
