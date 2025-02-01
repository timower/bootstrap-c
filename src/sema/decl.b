import state;
import type;
import utils;

import stmt;

func semaDecl(state: SemaState*, decl: DeclAST*) {
  switch (decl->kind) {
    case DeclKind::STRUCT:
      // Resolve tags in fields.
      for (let field = decl->fields; field != null; field = field->next) {
        if (field->kind != DeclKind::VAR) {
          failSemaDecl(field, "Only var decls allowed in struct");
        }

        resolveTypeTags(state, field->type, field->location);
      }
    case DeclKind::ENUM:
      break;
    case DeclKind::UNION:
      let maxSize = 0;
      for (let tag = decl->subTypes; tag != null; tag = tag->next) {
        // sema the 'tag' which is a struct.
        semaDecl(state, tag->decl);

        let size = getStructDeclSize(state, tag->decl);
        if (size > maxSize) {
          maxSize = size;
        }
      }
      decl->enumValue = maxSize;
      break;
    case DeclKind::FUNC:
      if (decl->body != null) {
        let funcState = SemaState {
          parent = state,
          result = (decl->type->kind as TypeKind::Func*)->result,
        };

        // Generate a local for each arg.
        for (let field = decl->fields; field != null; field = field->next) {
          resolveTypeTags(state, field->type, field->location);
          addLocalDecl(&funcState, field);
        }
        semaStmt(&funcState, decl->body);
      }
    case DeclKind::VAR:
      semaVarDecl(state, decl);

    case DeclKind::IMPORT:
      break;

    case DeclKind::ENUM_FIELD:
      failSemaDecl(decl, "Shoudln't happen");
      return;
  }
}
