import ast;
import type;
import lsp;

func getFields(decl: DeclAST*) -> DeclAST* {
  switch (decl->kind) {
    case DeclKind::Struct as structKind:
      return structKind.fields;
    case DeclKind::Enum as enumKind:
      return enumKind.fields;
    default:
      unreachable("Unsupported type to get field of");
      return null;
  }
}

func findField(
    state: SemaState*,
    structDecl: DeclAST*,
    name: Token,
    idxOut: i32*
) -> DeclAST* {
  let idx = 0;
  let fields = getFields(structDecl);
  for (let field = fields; field != null; field = field->next, idx++) {
    if (tokCmp(name, field->name)) {
      *idxOut = idx;

      if (state->semaLspMode) {
        lspRef(field, &name);
      }
      return field;
    }
  }

  return null;
}


// Finds a struct type in the state. Correctly looks for unions tags as well.
func lookupStruct(state: SemaState*, type: TypeKind::Struct*) -> DeclAST* {
  // Check for union as parent
  if (type->parent != null) {
    let tag = getTypeTag(type->parent);
    if (tag == null) {
      unreachable("Parent without tag?");
    }
    let unionDecl = lookupType(state, *tag);
    if (unionDecl == null) {
      // Resolving type tags should fail before this.
      unreachable("Couldn't find union");
    }

    return findType((&unionDecl->kind as DeclKind::Union*)->subTypes, type->tag);
  }
  return lookupType(state, type->tag);
}


func findLocal(local: DeclList*, name: Token) -> DeclAST* {
  for (; local != null; local = local->next) {
    if (tokCmp(name, local->decl->name)) {
      return local->decl;
    }
  }
  return null;
}

func lookupLocal(state: SemaState*, name: Token) -> DeclAST* {
  for (; state != null; state = state->parent) {
    let local = findLocal(state->locals, name);
    if (local != null) {
      if (state->semaLspMode) {
        lspRef(local, &name);
      }
      return local;
    }
  }

  return null;
}

func addLocalDecl(state: SemaState*, decl: DeclAST*) {
  let prev = findLocal(state->locals, decl->name);
  if (prev != null) {
    failSemaDecl(state, decl, "Variable redef");
  }

  if (state->semaLspMode) {
    let name = decl->name;
    printLoc(name.location);
    fprintf(getStderr(), "decl: %p: %.*s\n", decl, name.data.len, &name.data[0]);
  }

  let newLocal = newDeclList(&state->localAlloc, decl);
  newLocal->next = state->locals;
  state->locals = newLocal;
}
