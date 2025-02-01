import ast;

func findField(
    structDecl: DeclAST*,
    name: Token,
    idxOut: i32*
) -> DeclAST* {
  let idx = 0;
  for (let field = structDecl->fields; field != null;
       field = field->next, idx++) {
    if (tokCmp(name, field->name)) {
      *idxOut = idx;
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
      return null;
    }
    let unionDecl = lookupType(state, *tag);
    if (unionDecl == null) {
      return null;
    }

    return findType(unionDecl->subTypes, type->tag);
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
      return local;
    }
  }

  return null;
}

func addLocalDecl(state: SemaState*, decl: DeclAST*) {
  let prev = findLocal(state->locals, decl->name);
  if (prev != null) {
    // Allow redef of functions, TODO: verify type match...
    if (prev->kind == DeclKind::FUNC && prev->body == null) {
      prev->hasDef = 1;
    } else {
      failSemaDecl(decl, "Variable redef");
    }
  }

  let newLocal = newDeclList(decl);
  newLocal->next = state->locals;
  state->locals = newLocal;
}
