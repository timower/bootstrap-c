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
      for (let field = funcKind.fields; field != null; field = field->next) {
        resolveTypeTags(state, field->type, field->location);
      }
    default:
      // Nothing to do for other decl types
      break;
  }
}

func getImportExprName(expr: ExprAST*) -> Token {
  switch (expr->kind) {
    case ExprKind::VARIABLE:
      return expr->identifier;
    case ExprKind::MEMBER:
      let lhsToken = getImportExprName(expr->lhs);
      let buf = malloc(256) as i8*;

      let end = sprintf(
          buf,
          "%.*s/%.*s",
          lhsToken.end - lhsToken.data,
          lhsToken.data,
          expr->identifier.end - expr->identifier.data,
          expr->identifier.data);

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
  let rootFile = strdup(decl->location.fileName);
  let rootDir = dirname(rootFile);

  let relPath: i8* = malloc(4096);

  let absPath: i8* = null;
  let lastDir = strdup(rootDir);
  while (true) {
    sprintf(relPath, "%s/%.*s.b", rootDir, name.end - name.data, name.data);
    absPath = realpath(relPath, null);
    if (absPath != null) {
      break;
    }

    sprintf(
        relPath,
        "%s/%.*s.%s.b",
        rootDir,
        name.end - name.data,
        name.data,
        state->target);
    absPath = realpath(relPath, null);
    if (absPath != null) {
      break;
    }

    rootDir = dirname(rootDir);

    // Check if 'rootDir' == '/'
    if (strcmp(lastDir, rootDir) == 0) {
      failSemaDecl(decl, "Couldn't find file");
    }

    lastDir = strdup(rootDir);
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
  for (let cur = decl; cur != null; cur = cur->next) {
    if (&cur->kind as DeclKind::Import* != null) {
      resolveImport(state, cur);
    }
  }

  for (let cur = decl; cur != null; cur = cur->next) {
    addTaggedType(state, cur);
  }

  for (let cur = decl; cur != null; cur = cur->next) {
    resolveDeclTypeTags(state, cur);
  }

  for (let cur = decl; cur != null; cur = cur->next) {
    if (&cur->kind as DeclKind::Func* != null) {
      addLocalDecl(state, cur);
    }
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

  return decl;
}
