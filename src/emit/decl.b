import state;
import type;
import utils;
import stmt;


func addGlobalDecl(state: EmitState*, decl: DeclAST*) {
  switch (decl->kind) {
    case DeclKind::FUNC:
      let val = getGlobal(decl->name);
      val.type = "ptr";
      addLocal(state, decl->name, val);

    default:
      break;
  }
}

func emitGlobalDecl(state: EmitState*, decl: DeclAST*) {
  switch (decl->kind) {
    case DeclKind::ENUM:
      return;
    case DeclKind::UNION:
      emitUnion(state, decl);
    case DeclKind::VAR:
      emitGlobalVar(state, decl);
    case DeclKind::STRUCT:
      emitStruct(state, decl);
    case DeclKind::FUNC:
      emitFunc(state, decl);
    case DeclKind::IMPORT:
      break;
    case DeclKind::ENUM_FIELD:
      failEmit("Unsupported");
  }
}

func emitFunc(state: EmitState*, decl: DeclAST*) {
  if (decl->hasDef && decl->body == null) {
    return;
  }

  let val = lookupVar(state, decl->name);

  let fnType = decl->type->kind as TypeKind::Func*;

  let defOrDecl = decl->body == null ? "declare" as i8* : "define" as i8*;
  printf("%s %s %s(", defOrDecl, convertType(fnType->result), val.val);
  for (let arg = decl->fields; arg != null; arg = arg->next) {
    let len = arg->name.end - arg->name.data;
    printf("%s %%%.*s", convertType(arg->type), len, arg->name.data);
    if (arg->next != null) {
      printf(", ");
    }
  }
  printf(")");

  if (decl->body != null) {
    let funcState = newEmitState(state);
    let funcCounter = 0;
    funcState.tmpCounter = &funcCounter;

    printf(" {\n");

    for (let arg = decl->fields; arg != null; arg = arg->next) {
      let addr = emitLocalVar(&funcState, arg);
      let len = arg->name.end - arg->name.data;
      printf(
          "  store %s %%%.*s, ptr %s\n",
          convertType(arg->type),
          len,
          arg->name.data,
          addr.val);
    }
    emitStmt(&funcState, decl->body);

    // Emit implict void return.
    if (fnType->result->kind as TypeKind::Void* != null) {
      printf("  ret void\n");
    } else {
      printf("  ret %s undef\n", convertType(fnType->result));
    }

    printf("}\n");
  } else {
    printf("\n");
  }
}

func emitStruct(state: EmitState*, decl: DeclAST*) {
  // emit nested structs
  for (let field = decl->fields; field != null; field = field->next) {
    if (field->kind == DeclKind::STRUCT) {
      emitStruct(state, field);
    }
  }

  // TODO: padding
  printf("%s = type <{ ", convertType(decl->type));

  for (let field = decl->fields; field != null; field = field->next) {
    printf("%s", convertType(field->type));
    if (field->next != null) {
      printf(", ");
    }
  }

  printf(" }>\n");
}

func emitUnion(state: EmitState*, decl: DeclAST*) {
  // emit nested structs
  for (let tag = decl->subTypes; tag != null; tag = tag->next) {
    if (tag->decl->kind != DeclKind::STRUCT) {
      failEmit("Expected struct tag in union");
    }
    emitStruct(state, tag->decl);
  }

  printf("%s = type <{ ", convertType(decl->type));

  // Add a kind tag in front as i32
  printf("i32, ");

  // Emit an array of i8s of the largest size set by sema
  // TODO: this won't work for alignment reasons, should be fixed before moving
  // away from packed structs.
  let size = decl->enumValue;
  printf("[%d x i8]", size);

  printf(" }>\n");
}

func emitGlobalVar(state: EmitState*, decl: DeclAST*) {
  let declSpec = decl->type->isConst ? "constant" as i8* : "global" as i8*;

  let val = getGlobal(decl->name);
  val.type = convertType(decl->type);
  if (decl->init != null) {
    let init = emitExpr(state, decl->init);    // TODO: emit constant
    printf("%s = %s %s %s\n", val.val, declSpec, init.type, init.val);
  } else {
    let init = decl->type->kind as TypeKind::Struct* != null
         ? "zeroinitializer" as i8*
         : "null" as i8*;
    printf("%s = %s %s %s\n", val.val, declSpec, val.type, init);
  }

  addLocal(state, decl->name, val);
}
