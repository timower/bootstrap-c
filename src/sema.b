import ast;
import ast.print;
import parse;

import sema.state;
import sema.type;
import sema.expr;
import sema.decl;

func addTaggedType(state: SemaState*, decl: DeclAST*) {
  if (&decl->kind as DeclKind::Struct* != null || &decl->kind as DeclKind::Enum* != null || &decl->kind as DeclKind::Union* != null) {
    if (findType(state->types, *getTypeTag(decl->type)) != null) {
      failSemaDecl(decl, ": Type redef");
    }

    // Add the struct to the types.
    let type = newDeclList(decl);
    type->next = state->types;
    state->types = type;
  }
}

func resolveDeclTypeTags(state: SemaState*, decl: DeclAST*) {
  resolveTypeTags(state, decl->type, decl->location);

  switch (decl->kind) {
    case DeclKind::Struct as structKind:
      // Resolve tags in fields.
      for (let field = structKind.fields; field != null; field = field->next) {
        resolveTypeTags(state, field->type, field->location);
      }
    case DeclKind::Union as unionKind:
      let maxSize = 0;
      for (let tag = unionKind.subTypes; tag != null; tag = tag->next) {
        // sema the 'tag' which is a struct.
        resolveDeclTypeTags(state, tag->decl);
      }
    case DeclKind::Func as funcKind:
      for (let arg = funcKind.args; arg != null; arg = arg->next) {
        resolveTypeTags(state, arg->type, arg->location);
      }
    default:
      // Nothing to do for other decl types
      break;
  }
}

func findCachedPath(state: SemaState*, importName: i8*) -> i8* {
  for (let cur = state->pathCache; cur != null; cur = cur->next) {
    if (strcmp(importName, cur->importName) == 0) {
      return cur->resolvedPath;
    }
  }
  return null;
}

func addToPathCache(state: SemaState*, importName: i8*, resolvedPath: i8*) {
  let cache: PathCache* = calloc(1, sizeof(struct PathCache));
  cache->importName = strdup(importName);
  cache->resolvedPath = strdup(resolvedPath);
  cache->next = state->pathCache;
  state->pathCache = cache;
}

func getImportExprName(expr: ExprAST*) -> Token {
  switch (expr->kind) {
    case ExprKind::Variable as varExpr:
      return varExpr.identifier;
    case ExprKind::Member as memberExpr:
      let lhsToken = getImportExprName(memberExpr.object);
      let buf = malloc(256) as i8*;

      let end = sprintf(
          buf,
          "%.*s/%.*s",
          lhsToken.end - lhsToken.data,
          lhsToken.data,
          memberExpr.identifier.end - memberExpr.identifier.data,
          memberExpr.identifier.data);

      return Token {
        kind = TokenKind::IDENTIFIER,
        data = buf,
        end = buf + end,
      };

    default:
      failSemaExpr(expr, "Unexpected expression in import");
  }
}

func resolveImport(state: SemaState*, decl: DeclAST*) {
  if (state->parent != null) {
    failSemaDecl(decl, "Import not allowed in local scope");
  }

  let name = getImportExprName((&decl->kind as DeclKind::Import*)->path);

  // Create cache key from import name, target, and source directory
  let cacheKey: i8* = malloc(512);
  let rootFile = strdup(decl->location.fileName);
  let rootDir = dirname(rootFile);
  sprintf(cacheKey, "%s:%.*s:%s", rootDir, name.end - name.data, name.data, state->target);

  // Check cache first for the resolved absolute path
  let cachedAbsPath = findCachedPath(state, cacheKey);
  let absPath: i8* = null;
  let relPath: i8* = null;

  if (cachedAbsPath != null) {
    absPath = cachedAbsPath;
    relPath = cachedAbsPath;    // Use absolute path for parsing too
  } else {
    // Not in cache, do full resolution
    relPath = malloc(4096);
    let lastDir = strdup(rootDir);
    while (true) {
      sprintf(relPath, "%s/%.*s.b", rootDir, name.end - name.data, name.data);
      if (access(relPath, F_OK) == 0) {
        absPath = realpath(relPath, null);
        break;
      }

      sprintf(
          relPath,
          "%s/%.*s.%s.b",
          rootDir,
          name.end - name.data,
          name.data,
          state->target);
      if (access(relPath, F_OK) == 0) {
        absPath = realpath(relPath, null);
        break;
      }

      rootDir = dirname(rootDir);

      // Check if 'rootDir' == '/'
      if (strcmp(lastDir, rootDir) == 0) {
        failSemaDecl(decl, "Couldn't find file");
      }

      lastDir = strdup(rootDir);
    }

    // Add resolved path to cache
    addToPathCache(state, cacheKey, absPath);
  }

  // Check if we already import this file.
  for (let cur = state->imports; cur != null; cur = cur->next) {
    if (strcmp(absPath, cur->name) == 0) {
      return;
    }
  }

  // Add to imports
  let imports: ImportList* = calloc(1, sizeof(struct ImportList));
  imports->name = absPath;
  imports->next = state->imports;
  state->imports = imports;

  let fileDecls = parseFile(relPath);
  if (fileDecls == null) {
    failSemaDecl(decl, "Failed to import file");
  }

  // semaTopLevel will return a combined list of decls from the file and the
  // extraDecls.
  let extras = semaTopLevel(state, fileDecls);
  state->extraDecls = extras;
}

func semaTopLevel(state: SemaState*, decl: DeclAST*) -> DeclAST* {
  let fileName = decl->location.fileName;

  // First resolve all imports.
  for (let cur = decl; cur != null; cur = cur->next) {
    if (&cur->kind as DeclKind::Import* != null) {
      resolveImport(state, cur);
    }
  }

  // Add all tagged types to the state, so we can solve them later.
  for (let cur = decl; cur != null; cur = cur->next) {
    addTaggedType(state, cur);
  }

  // Add all functions, so typeof(func) works.
  for (let cur = decl; cur != null; cur = cur->next) {
    if (&cur->kind as DeclKind::Func* != null) {
      addLocalDecl(state, cur);
    }
  }

  // Reolve tagged types to struct / union and lower typeof().
  for (let cur = decl; cur != null; cur = cur->next) {
    resolveDeclTypeTags(state, cur);
  }

  // Do actual type checking and AST transformations.
  for (let cur = decl; cur != null; cur = cur->next) {
    semaDecl(state, cur);
  }

  // Add extra decls
  if (state->extraDecls != null) {
    let last = state->extraDecls;
    while (last->next != null) {
      last = last->next;
    }
    last->next = decl;
    decl = state->extraDecls;
  }

  // Sema successful, report LSP info in case
  if (state->semaLspMode) {
    fprintf(getStderr(), "%s:%d:%d: OK!\n", fileName, 0, 0);
  }
  return decl;
}
