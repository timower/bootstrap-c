import ast;
import ast.print;

import generics;

import target;

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
  target: Target;

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

  // For generics
  instanceCounter: i32;
  genericInstances: GenericInst*;

  // Used to give each string a unique name.
  strCount: i32;

  // List of files imported.
  imports: ImportList*;

  // Cache of loaded paths to reduce realpath use.
  pathCache: PathCache*;

  // Set to true to give LSP related output during sema.
  semaLspMode: bool;

  // Set to true by failSema to make sure we can report all errors.
  failed: bool;

  // Used to check all paths in a function return.
  returns: bool;

  // Depth tracker, to prevent infinite generic instantiations.
  depth: i32;

  jmpBuf: JmpBuf*;
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
func failSema(state: SemaState*, loc: SourceLoc*, msg: const i8*) {
  printLoc(loc);
  fprintf(getStderr(), "sema error: %s\n", msg);
  if (state == null) {
    exit(1);
  }
  longjmp(getRoot(state)->jmpBuf, 1);
}

func errorSema(state: SemaState*, loc: SourceLoc*, msg: const i8*) {
  printLoc(loc);
  fprintf(getStderr(), "sema error: %s\n", msg);
  getRoot(state)->failed = true;
}

func failSemaType(state: SemaState*, type: Type*, msg: const i8*) {
  failSema(state, type->location, msg);
}

func failSemaExpr(state: SemaState*, expr: ExprAST*, msg: const i8*) {
  failSema(state, expr->location, msg);
}

func failSemaDecl(state: SemaState*, decl: DeclAST*, msg: const i8*) {
  failSema(state, decl->location, msg);
}

func failSemaStmt(state: SemaState*, stmt: StmtAST*, msg: const i8*) {
  failSema(state, stmt->location, msg);
}

func getRoot(state: SemaState*) -> SemaState* {
  while (state->parent != null) {
    state = state->parent;
  }
  return state;
}

func getNullDecl() -> DeclAST* {
  let nullTok = newInternalToken(4);
  memcpy(&nullTok.data[0], "null", 4);

  // Add null as a nullptr
  let nullDecl = newDecl(DeclKind::EnumField {
    enumValue = 0,
  });
  nullDecl->name = nullTok;
  nullDecl->type = newType(TypeKind::Pointer {
    pointee = newType(TypeKind::Void {}),
  });

  return nullDecl;
}

func getTargetDecl(target: Target*) -> DeclAST* {
  let targetTok = newInternalToken(8);
  memcpy(&targetTok.data[0], "_TARGET_", 8);
  let initVal = newInternalToken(target->triple.len as uptr);
  memcpy(&initVal.data[0], &target->triple[0], target->triple.len as uptr);

  let init = newExpr(ExprKind::Str {
    identifier = initVal,
  });
  init->type = newType(TypeKind::Array {
    element = getCharType(),
    size = target->triple.len as i32 + 1,
  });

  let decl = newDecl(DeclKind::Var {
    init = init,
  });
  decl->type = newType(TypeKind::Array {
    element = getCharType(),
    size = target->triple.len as i32,
  });
  decl->name = targetTok;

  return decl;
}

func initSemaState(target: Target, lspMode: bool) -> SemaState {
  let nullDecl = getNullDecl();
  let targetDecl = getTargetDecl(&target);

  let locals = newDeclList(nullDecl);
  locals->next = newDeclList(targetDecl);

  return SemaState {
    target = target,
    locals = locals,
    semaLspMode = lspMode,
    extraDecls = targetDecl,
  };
}
