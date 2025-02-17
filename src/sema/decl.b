import state;
import type;
import utils;

import stmt;

func semaDecl(state: SemaState*, decl: DeclAST*) {
  switch (decl->kind) {
    case DeclKind::STRUCT:
      for (let field = decl->fields; field != null; field = field->next) {
        if (field->kind != DeclKind::VAR) {
          failSemaDecl(field, "Only var decls allowed in struct");
        }
      }

    case DeclKind::UNION:
      let maxSize = 0;
      for (let tag = decl->subTypes; tag != null; tag = tag->next) {
        let size = getStructDeclSize(state, tag->decl);
        if (size > maxSize) {
          maxSize = size;
        }
      }
      decl->enumValue = maxSize;

    case DeclKind::FUNC:
      if (decl->body != null) {
        let funcState = SemaState {
          parent = state,
          result = (decl->type->kind as TypeKind::Func*)->result,
        };

        // Generate a local for each arg.
        for (let field = decl->fields; field != null; field = field->next) {
          addLocalDecl(&funcState, field);
        }
        semaStmt(&funcState, decl->body);
      }

    case DeclKind::VAR:
      semaVarDecl(state, decl);

    case DeclKind::ENUM:
      break;
    case DeclKind::IMPORT:
      break;

    case DeclKind::ENUM_FIELD:
      failSemaDecl(decl, "Shoudln't happen");
      return;
  }
}
