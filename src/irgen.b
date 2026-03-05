import ast;
import ir;
import ir.type;

import irgen.state;
import irgen.expr;
import irgen.stmt;

func genModule(
    allocator: Allocator*,
    decls: DeclAST*,
    target: Target
) -> Module* {
  let localAlloc = Allocator {};
  defer freeAll(&localAlloc);
  let state = IRGenState {
    irAlloc = allocator,
    localAlloc = &localAlloc,
  };

  state.module = alloc(allocator, sizeof(Module)) as Module*;

  state.jmpBuf = newJmpBuf();
  defer free(state.jmpBuf);
  if (setjmp(state.jmpBuf) != 0) {
    return null;
  }

  state.module->target = target;

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
          unreachable("Expected to find function");
        }

        let fnPtr = fun as Value::FuncPtr*;

        genFunc(&state, cur, fnPtr->ptr);
      }
    }
  }

  popScope(&state);
  if (state.scope != null) {
    unreachable("Scope push & pop mismatch");
  }

  return state.module;
}

func getDeclIRName(a: Allocator*, ident: [i8]) -> [i8] {
  let len = ident.len as i32;
  let buf: i8* = alloc(a, len + 2);
  len = sprintf(buf, "@%.*s", len, &ident[0]);
  return buf[:len];
}

func addGlobal(state: IRGenState*, decl: DeclAST*) -> Value {
  let global = newGlobal(state->irAlloc);
  global->name = getDeclIRName(state->irAlloc, decl->name.data);
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

  global->next = state->module->globals;
  state->module->globals = global;

  return Value::GlobalPtr {
    ptr = global,
  };
}

func addFunc(state: IRGenState*, decl: DeclAST*) -> Value {
  let fn = newFunction(state->irAlloc);
  fn->name = getDeclIRName(state->irAlloc, decl->name.data);
  fn->type = decl->type;

  fn->next = state->module->functions;
  state->module->functions = fn;

  return Value::FuncPtr {
    ptr = fn,
  };
}

func addStruct(state: IRGenState*, decl: DeclAST*) {
  let irStruct = newIRStruct(state->irAlloc);

  irStruct->name = convertType(state->irAlloc, decl->type);
  let typePtr = &irStruct->fields;
  for (let field = (&decl->kind as DeclKind::Struct*)->fields; field != null; field = field->next) {
    *typePtr = field->type;
    typePtr = &field->type->next;
  }

  irStruct->next = state->module->types;
  state->module->types = irStruct;
}

func addUnion(state: IRGenState*, decl: DeclAST*) {
  // emit nested structs
  for (let tag = (&decl->kind as DeclKind::Union*)->subTypes; tag != null; tag = tag->next) {
    addStruct(state, tag->decl);
  }

  let irStruct = newIRStruct(state->irAlloc);

  irStruct->name = convertType(state->irAlloc, decl->type);

  // Add type tag field
  irStruct->fields = getInt32(state->irAlloc);
  let tagBuffer = newType(state->irAlloc, TypeKind::Array {
    element = getCharType(state->irAlloc),
    size = (&decl->kind as DeclKind::Union*)->maxSize,
  });
  irStruct->fields->next = tagBuffer;

  irStruct->next = state->module->types;
  state->module->types = irStruct;
}

func createIntrinsics(state: IRGenState*) {
  // memcpy
  let args = getPtrType(state->irAlloc);
  args->next = getPtrType(state->irAlloc);
  args->next->next = getInt32(state->irAlloc);
  args->next->next->next = getBool(state->irAlloc);

  let fnCpy = newFunction(state->irAlloc);
  fnCpy->name = "@llvm.memcpy.p0.p0.i32";
  fnCpy->type = newType(state->irAlloc, TypeKind::Func {
    result = newType(state->irAlloc, TypeKind::Void {}),
    args = args,
    isVarargs = false,
  });

  fnCpy->next = state->module->functions;
  state->module->functions = fnCpy;
  state->intrinsics.memcpy = fnCpy;

  let fnTrap = newFunction(state->irAlloc);
  fnTrap->name = "@llvm.trap";
  fnTrap->type = newType(state->irAlloc, TypeKind::Func {
    result = newType(state->irAlloc, TypeKind::Void {}),
    isVarargs = false,
  });

  fnTrap->next = state->module->functions;
  state->module->functions = fnTrap;
  state->intrinsics.trap = fnTrap;

  // slice type
  let irStruct = newIRStruct(state->irAlloc);

  irStruct->name = "%slice";
  irStruct->fields = getPtrType(state->irAlloc);
  irStruct->fields->next = getIPtr(state->irAlloc, &state->module->target);
  irStruct->next = state->module->types;
  state->module->types = irStruct;
}
