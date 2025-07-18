import state;
import type;
import utils;

import stmt;

func semaDecl(state: SemaState*, decl: DeclAST*) {
  switch (decl->kind) {
    case DeclKind::Struct as structKind:
      for (let field = structKind.fields; field != null; field = field->next) {
        if (&field->kind as DeclKind::Var* == null) {
          failSemaDecl(field, "Only var decls allowed in struct");
        }
      }
    case DeclKind::Union as unionKind:
      let maxSize = 0;
      for (let tag = unionKind.subTypes; tag != null; tag = tag->next) {
        let size = getStructDeclSize(state, tag->decl);
        if (size > maxSize) {
          maxSize = size;
        }
      }
      unionKind.maxSize = maxSize;
    case DeclKind::Func as funcKind:
      if (funcKind.body != null) {
        let funcState = newState(state);
        funcState.result = (decl->type->kind as TypeKind::Func*)->result;

        // Generate a local for each arg.
        for (let arg = funcKind.args; arg != null; arg = arg->next) {
          addLocalDecl(&funcState, arg);
        }
        semaStmt(&funcState, funcKind.body);
      }
    case DeclKind::Var:
      semaVarDecl(state, decl);
    case DeclKind::Const:
      semaVarDecl(state, decl);
    case DeclKind::Enum:
      // Nothing to do for enums
      break;
    case DeclKind::Import:
      // Nothing to do for imports
      break;
    case DeclKind::EnumField:
      failSemaDecl(decl, "Shoudln't happen");
      return;
  }
}
