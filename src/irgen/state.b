import ir;

struct IRGenState {
  module: Module*;

  // Allocator for anything that will be returned as IR.
  irAlloc: Allocator*;

  // Allocator for local things (scopes, locals, ...).
  localAlloc: Allocator*;

  curFunc: Function*;
  curBB: BasicBlock*;

  // Set per function on first defer.
  cleanupSlot: Value;
  cleanupCounter: i32;

  // Used for basic blocks.
  globalCounter: i32;

  // Used for instructions.
  counter: i32;

  scope: Scope*;

  intrinsics: Intrinsics;

  jmpBuf: JmpBuf*;
}

struct Cleanup {
  bb: BasicBlock*;
  stmt: StmtAST*;
  cases: Case*;

  next: Cleanup*;
}

struct JmpSlot {
  bb: BasicBlock*;

  // Scope this will jump to. Cleanups up to but not including are executed.
  scope: Scope*;
}

struct Scope {
  locals: Local*;

  breakSlot: JmpSlot;
  continueSlot: JmpSlot;

  cleanups: Cleanup*;

  parent: Scope*;
}

struct Local {
  name: Token;
  value: Value;

  next: Local*;
}

struct Intrinsics {
  memcpy: Function*;
  trap: Function*;
}

func failIRGen(state: IRGenState*, msg: i8*) {
  fprintf(getStderr(), "irgen fail: %s\n", msg);
  longjmp(state->jmpBuf, 1);
}

func newScope(state: IRGenState*) {
  let scope = alloc(state->localAlloc, sizeof(struct Scope)) as Scope*;
  if (state->scope != null) {
    scope->breakSlot = state->scope->breakSlot;
    scope->continueSlot = state->scope->continueSlot;
  }
  scope->parent = state->scope;
  state->scope = scope;
}


func addLocal(state: IRGenState*, name: Token, value: Value) {
  let local = alloc(state->localAlloc, sizeof(struct Local)) as Local*;
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
  unreachable("IRGen name not found, probably sema bug?");
  return null;
}

func addAlloca(state: IRGenState*, type: Type*) -> Value {
  let res = alloc(state->irAlloc, sizeof(struct Alloca)) as Alloca*;
  res->name = state->counter++;
  res->type = type;
  res->dbgName = "alloc";

  res->next = state->curFunc->allocs;
  state->curFunc->allocs = res;

  return Value::AllocaPtr {
    ptr = res,
  };
}

func addBasicBlock(
    state: IRGenState*,
    label: i8*,
    sourceLoc: SourceLoc*
) -> BasicBlock* {
  let fn = state->curFunc;
  let res = alloc(state->irAlloc, sizeof(struct BasicBlock)) as BasicBlock*;
  res->label = label;
  res->name = state->globalCounter++;
  res->location = sourceLoc;

  if (fn->end == null) {
    fn->begin = res;
    fn->end = res;
  } else {
    fn->end->next = res;
    res->prev = fn->end;
    fn->end = res;
  }
  return res;
}

func newInstr(state: IRGenState*, kind: InstrKind) -> Instruction* {
  let res = alloc(state->irAlloc, sizeof(struct Instruction)) as Instruction*;
  res->kind = kind;
  return res;
}

func addInstr(state: IRGenState*, type: Type*, kind: InstrKind) -> Value {
  let res = newInstr(state, kind);
  res->type = type;
  res->name = state->counter++;

  let bb = state->curBB;
  if (bb->end == null) {
    bb->begin = res;
    bb->end = res;
  } else {
    bb->end->next = res;
    res->prev = bb->end;
    bb->end = res;
  }

  return Value::InstrPtr {
    ptr = res,
  };
}
