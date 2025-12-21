import ast;
import ir;
import ir.type;

import irgen.state;
import irgen.expr;
import irgen.stmt;

func genModule(decls: DeclAST*, target: Target) -> Module {
  let state = IRGenState {};
  state.module.target = target;

  newScope(&state);
  createIntrinsics(&state);

  for (let cur = decls; cur != null; cur = cur->next) {
    if (!isGeneric(cur)) {
      switch (cur->kind) {
        case DeclKind::Func as funcKind:
          // Add all functions so we can use before decl.
          let global = addFunc(&state, cur);
          addLocal(&state, cur->name, global);
        case DeclKind::Struct:
          // Add typedefs for struct and unions types.
          addStruct(&state, cur);
        case DeclKind::Union:
          addUnion(&state, cur);
        case DeclKind::Var:
          // add Globals
          let global = addGlobal(&state, cur);
          addLocal(&state, cur->name, global);
        default:
          // Nothing to do for other decl types in first pass
          break;
      }
    }
  }

  for (let cur = decls; cur != null; cur = cur->next) {
    if (let funcKind = &cur->kind as DeclKind::Func*) {
      if (!isGeneric(cur) && funcKind->body != null) {
        let fun = findName(&state, cur->name);
        if (fun == null) {
          failIRGen("Expected to find function");
        }

        let fnPtr = fun as Value::FuncPtr*;

        genFunc(&state, cur, fnPtr->ptr);
      }
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
  let len = ident.len as iptr;
  let buf: i8* = malloc((len + 2) as uptr);
  sprintf(buf, "@%.*s", len, ident.data);

  let global = newGlobal();
  global->name = buf;
  global->type = decl->type;

  let varKind = &decl->kind as DeclKind::Var*;
  if (varKind->init != null) {
    global->init = genConstant(state, varKind->init);
  } else if (!varKind->isExtern) {
    global->init = Value::Zero {
      type = decl->type,
    };
  } else {
    global->isExtern = true;
  }

  global->next = state->module.globals;
  state->module.globals = global;

  return Value::GlobalPtr {
    ptr = global,
  };
}

func addFunc(state: IRGenState*, decl: DeclAST*) -> Value {
  let ident = decl->name;
  let len = ident.len as iptr;
  let buf: i8* = malloc((len + 2) as uptr);
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
  for (let field = (&decl->kind as DeclKind::Struct*)->fields; field != null; field = field->next) {
    *typePtr = field->type;
    typePtr = &field->type->next;
  }

  irStruct->next = state->module.types;
  state->module.types = irStruct;
}

func addUnion(state: IRGenState*, decl: DeclAST*) {
  // emit nested structs
  for (let tag = (&decl->kind as DeclKind::Union*)->subTypes; tag != null; tag = tag->next) {
    addStruct(state, tag->decl);
  }

  let irStruct = newIRStruct();

  irStruct->name = convertType(decl->type);

  // Add type tag field
  irStruct->fields = getInt32();
  let tagBuffer = newType(TypeKind::Array {
    element = getCharType(),
    size = (&decl->kind as DeclKind::Union*)->maxSize,
  });
  irStruct->fields->next = tagBuffer;

  irStruct->next = state->module.types;
  state->module.types = irStruct;
}

func createIntrinsics(state: IRGenState*) {
  // memcpy
  let args = getPtrType();
  args->next = getPtrType();
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

  // slice type
  let irStruct = newIRStruct();

  irStruct->name = "%slice";
  irStruct->fields = getPtrType();
  irStruct->fields->next = getIPtr(&state->module.target);

  irStruct->next = state->module.types;
  state->module.types = irStruct;
}
