import parse;
import sema.state;

import debug;


// Find the stdlib path and cache it in the 'stdlibPath' global.
// Search paths are:
//  - binary_path/../stdlib/
//  - binary_path/../../lib/brio
func findStdlib(state: SemaState*) {
  let selfPath = getSelfPath(&state->localAlloc);
  debug(&selfPath[0]);

  let selfDir = dirname(&selfPath[0]);

  let result = alloc(&state->localAlloc, 4096);
  sprintf(result, "%s/%s", selfDir, "stdlib");
  if (access(result, F_OK) == 0) {
    sprintf(result, "%s", selfDir);
    state->stdlibPath = result;
    return;
  }

  selfDir = dirname(selfDir);
  sprintf(result, "%s/%s", selfDir, "lib/brio");
  if (access(result, F_OK) == 0) {
    state->stdlibPath = result;
    return;
  }

  failSema(state, null, "Can't find stdlib");
}

func isStdlibImport(expr: ExprAST*) -> bool {
  switch (expr->kind) {
    case ExprKind::Variable as varExpr:
      return tokCmpStr(varExpr.identifier, "stdlib");

    case ExprKind::Member as memberExpr:
      return isStdlibImport(memberExpr.object);

    default:
      unreachable("Unexpected expression in import");
      return false;
  }
}

func getImportExprName(state: SemaState*, expr: ExprAST*) -> Token {
  switch (expr->kind) {
    case ExprKind::Variable as varExpr:
      return varExpr.identifier;
    case ExprKind::Member as memberExpr:
      let lhsToken = getImportExprName(state, memberExpr.object);
      let res = newInternalToken(
          &state->localAlloc,
          (lhsToken.data.len + memberExpr.identifier.data.len + 10) as uptr);
      let len = sprintf(
          &res.data[0],
          "%.*s/%.*s",
          lhsToken.data.len,
          &lhsToken.data[0],
          memberExpr.identifier.data.len,
          &memberExpr.identifier.data[0]);
      res.data = res.data[:len];
      return res;

    default:
      unreachable("Unexpected expression in import");
      return Token {};
  }
}

func checkPath(target: Target*, rootDir: i8*, name: Token, outPath: i8*) -> bool {
  sprintf(outPath, "%s/%.*s.b", rootDir, name.data.len, &name.data[0]);
  if (access(outPath, F_OK) == 0) {
    return true;
  }

  let targetName = getImportName(target);
  sprintf(
      outPath,
      "%s/%.*s.%.*s.b",
      rootDir,
      name.data.len,
      &name.data[0],
      targetName.len,
      &targetName[0]);
  if (access(outPath, F_OK) == 0) {
    return true;
  }

  return false;
}

func getImportPath(state: SemaState*, decl: DeclAST*) -> i8* {
  let importDecl = decl->kind as DeclKind::Import*;

  let name = getImportExprName(state, importDecl->path);

  // Paths are stored in the AST locations, so use the ast alloc.
  let outPath = alloc(state->astAlloc, 4096);

  if (isStdlibImport(importDecl->path)) {
    if (!checkPath(&state->target, state->stdlibPath, name, outPath)) {
      failSemaDecl(state, decl, "Couldn't find stdlib file");
      return null;
    }

    return outPath;
  }

  let rootFile = strdup(decl->location->fileName);
  defer free(rootFile);
  let rootDir = dirname(rootFile);

  let lastLen = strlen(rootDir);
  while (true) {
    if (checkPath(&state->target, rootDir, name, outPath)) {
      break;
    }

    rootDir = dirname(rootDir);
    let newLen = strlen(rootDir);

    // Check if 'rootDir' == '/'
    if (lastLen == newLen) {
      free(rootFile);
      failSemaDecl(state, decl, "Couldn't find file");
      return null;
    }

    lastLen = newLen;
  }

  return outPath;
}
