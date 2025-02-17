import ast;
import ir;
import ir.type;

import irgen.state;
import irgen.expr;
import irgen.stmt;

func genModule(decls: DeclAST*) -> Module {
  let state = IRGenState {};
  newScope(&state);
  createIntrinsics(&state);

  for (let cur = decls; cur != null; cur = cur->next) {
    switch (cur->kind) {
      // Add all functions so we can use before decl.
      case DeclKind::FUNC:
        let global = addFunc(&state, cur);
        addLocal(&state, cur->name, global);

      // Add typedefs for struct and unions types.
      case DeclKind::STRUCT:
        addStruct(&state, cur);
      case DeclKind::UNION:
        addUnion(&state, cur);

      // add Globals
      case DeclKind::VAR:
        let global = addGlobal(&state, cur);
        addLocal(&state, cur->name, global);

      default:
        break;
    }
  }

  for (let cur = decls; cur != null; cur = cur->next) {
    if (cur->kind == DeclKind::FUNC && cur->body != null) {
      let fun = findName(&state, cur->name);
      if (fun == null) {
        failIRGen("Expected to find function");
      }

      let fnPtr = fun as Value::FuncPtr*;

      genFunc(&state, cur, fnPtr->ptr);
    }
  }

  popScope(&state);
  if (state.scope != null) {
    failIRGen("Scope push & pop mismatch");
  }

  return state.module;
}

func addGlobal(state: IRGenState*, decl: DeclAST*) -> Value {
  // TODO: dedup
  let ident = decl->name;
  let len = ident.end - ident.data;
  let buf: i8* = malloc((len + 2) as u64);
  sprintf(buf, "@%.*s", len, ident.data);

  let global = newGlobal();
  global->name = buf;
  global->type = decl->type;
  if (decl->init != null) {
    global->init = genConstant(state, decl->init);
  } else {
    global->init = Value::Zero {
      type = decl->type,
    };
  }

  global->next = state->module.globals;
  state->module.globals = global;

  return Value::GlobalPtr {
    ptr = global,
  };
}

func addFunc(state: IRGenState*, decl: DeclAST*) -> Value {
  let ident = decl->name;
  let len = ident.end - ident.data;
  let buf: i8* = malloc((len + 2) as u64);
  sprintf(buf, "@%.*s", len, ident.data);

  let fn = newFunction();
  fn->name = buf;
  fn->type = decl->type;

  fn->next = state->module.functions;
  state->module.functions = fn;

  return Value::FuncPtr {
    ptr = fn,
  };
}

func addStruct(state: IRGenState*, decl: DeclAST*) {
  let irStruct = newIRStruct();

  irStruct->name = convertType(decl->type);
  let typePtr = &irStruct->fields;
  for (let field = decl->fields; field != null; field = field->next) {
    *typePtr = field->type;
    typePtr = &field->type->next;
  }

  irStruct->next = state->module.types;
  state->module.types = irStruct;
}

func addUnion(state: IRGenState*, decl: DeclAST*) {
  // emit nested structs
  for (let tag = decl->subTypes; tag != null; tag = tag->next) {
    addStruct(state, tag->decl);
  }

  let irStruct = newIRStruct();

  irStruct->name = convertType(decl->type);

  // Add type tag field
  irStruct->fields = getInt32();
  let tagBuffer = newType(TypeKind::Array {
    element = getCharType(),
    size = decl->enumValue,
  });
  irStruct->fields->next = tagBuffer;

  irStruct->next = state->module.types;
  state->module.types = irStruct;
}

func createIntrinsics(state: IRGenState*) {
  // memcpy
  let args = newType(TypeKind::Pointer {});
  args->next = newType(TypeKind::Pointer {});
  args->next->next = getInt32();
  args->next->next->next = getBool();

  let fn = newFunction();
  fn->name = "@llvm.memcpy.p0.p0.i32";
  fn->type = newType(TypeKind::Func {
    result = newType(TypeKind::Void {}),
    args = args,
    isVarargs = false,
  });

  fn->next = state->module.functions;
  state->module.functions = fn;
  state->intrinsics.memcpy = fn;
}
