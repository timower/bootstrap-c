import ir;

struct IRGenState {
  module: Module;

  curFunc: Function*;
  curBB: BasicBlock*;

  counter: i32;
  scope: Scope*;

  intrinsics: Intrinsics;
};

struct Scope {
  locals: Local*;
  breakBB: BasicBlock*;

  parent: Scope*;
};

struct Local {
  name: Token;
  value: Value;

  next: Local*;
};

struct Intrinsics {
  memcpy: Function*;
};

func failIRGen(msg: i8*) {
  printf("irgen fail: %s\n", msg);
  exit(1);
}

func newScope(state: IRGenState*) {
  let scope = calloc(1, sizeof(struct Scope)) as Scope*;
  if (state->scope != null) {
    scope->breakBB = state->scope->breakBB;
  }
  scope->parent = state->scope;
  state->scope = scope;
}

func popScope(state: IRGenState*) {
  state->scope = state->scope->parent;
}

func addLocal(state: IRGenState*, name: Token, value: Value) {
  let local = calloc(1, sizeof(struct Local)) as Local*;
  local->name = name;
  local->value = value;
  local->next = state->scope->locals;
  state->scope->locals = local;
}

func findName(state: IRGenState*, name: Token) -> Value* {
  for (let scope = state->scope; scope != null; scope = scope->parent) {
    for (let local = scope->locals; local != null; local = local->next) {
      if (tokCmp(local->name, name)) {
        return &local->value;
      }
    }
  }
  return null;
}

func addAlloca(state: IRGenState*, type: Type*) -> Value {
  let res = calloc(1, sizeof(struct Alloca)) as Alloca*;
  res->name = state->counter++;
  res->type = type;

  res->next = state->curFunc->allocs;
  state->curFunc->allocs = res;

  return Value::AllocaPtr {
    ptr = res,
  };
}

func addBasicBlock(state: IRGenState*, label: i8*) -> BasicBlock* {
  let fn = state->curFunc;
  let res = calloc(1, sizeof(struct BasicBlock)) as BasicBlock*;
  res->label = label;
  res->name = state->counter++;

  if (fn->end == null) {
    fn->begin = res;
    fn->end = res;
  } else {
    fn->end->next = res;
    fn->end = res;
  }
  return res;
}

func addInstr(state: IRGenState*, type: Type*, kind: InstrKind) -> Value {
  let res = calloc(1, sizeof(struct Instruction)) as Instruction*;
  res->kind = kind;
  res->name = state->counter++;
  res->type = type;

  let bb = state->curBB;
  if (bb->end == null) {
    bb->begin = res;
    bb->end = res;
  } else {
    bb->end->next = res;
    bb->end = res;
  }

  return Value::InstrPtr {
    ptr = res,
  };
}
