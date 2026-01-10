import ast;

import state;


func lspDecl(ptr: void*, tag: Token*) {
  printLoc(tag->location);
  fprintf(getStderr(), "decl: %p: %.*s\n", ptr, tag->data.len, &tag->data[0]);
}

func lspRef(ptr: void*, tag: Token*) {
  printLoc(tag->location);
  fprintf(getStderr(), "ref: %p: %d\n", ptr, tag->data.len);
}

func dumpDecl(decl: DeclAST*) {
  let tag = getTypeTag(decl->type);
  lspDecl(decl, tag);

  switch (decl->kind) {
    case DeclKind::Struct as structKind:
      for (let field = structKind.fields; field != null; field = field->next) {
        lspDecl(field, &field->name);
      }
    case DeclKind::Union as unionKind:
      let maxSize = 0;
      for (let tag = unionKind.subTypes; tag != null; tag = tag->next) {
        dumpDecl(tag->decl);
      }
    case DeclKind::Enum as enumKind:
      for (let field = enumKind.fields; field != null; field = field->next) {
        lspDecl(field, &field->name);
      }

    default:
      unreachable("Non dumpable declaration");
  }
}
