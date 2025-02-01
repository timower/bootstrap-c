import ast;
import ast.print;

struct EmitState {
  tmpCounter: i32*;
  vars: LocalVar*;

  curBreakLabel: i8*;
  switchUnionAddr: Value;

  parent: EmitState*;
};


// LLVM IR Value
struct Value {
  type: const i8*;

  // reg name or just the value
  val: const i8*;
};

struct LocalVar {
  name: Token;
  value: Value;

  next: LocalVar*;
};

struct Case {
  val: Value;
  n: i32;

  next: Case*;
};

func failEmit(msg: const i8*) {
  puts(msg);
  exit(1);
}

func failEmitLoc(loc: SourceLoc, msg: const i8*) {
  printf("%s:%d:%d: emit error: %s\n", loc.fileName, loc.line, loc.column, msg);
  exit(1);
}

func failEmitExpr(expr: ExprAST*, msg: const i8*) {
  failEmitLoc(expr->location, msg);
}

func newEmitState(parent: EmitState*) -> EmitState {
  let state = EmitState {
    tmpCounter = parent->tmpCounter,
    parent = parent,
    curBreakLabel = parent->curBreakLabel,
    switchUnionAddr = parent->switchUnionAddr,
  };

  return state;
}

func getCount(state: EmitState*) -> i32 {
  return (*state->tmpCounter)++;
}

func getNextTemp(state: EmitState*) -> Value {
  let buf: i8* = malloc(16);
  sprintf(buf, "%%tmp%d", getCount(state));

  return Value {
    val = buf,
  };
}

func getGlobal(ident: Token) -> Value {
  let len = ident.end - ident.data;
  let buf: i8* = malloc((len + 2) as u64);
  sprintf(buf, "@%.*s", len, ident.data);

  return Value {
    val = buf,
  };
}

func getTempGlobal(state: EmitState*, prefix: const i8*) -> Value {
  let buf: i8* = malloc(64);
  sprintf(buf, "@%s%d", prefix, getCount(state));

  return Value {
    val = buf,
  };
}

func newLocal(name: Token, val: Value) -> LocalVar* {
  let local: LocalVar* = calloc(1, sizeof(struct LocalVar));
  local->name = name;
  local->value = val;
  return local;
}

func addLocal(state: EmitState*, name: Token, val: Value) {
  let local = newLocal(name, val);
  local->next = state->vars;
  state->vars = local;
}

func lookupVar(state: EmitState*, tok: Token) -> Value {
  for (let local = state->vars; local != null; local = local->next) {
    if (tokCmp(tok, local->name)) {
      return local->value;
    }
  }

  if (state->parent != null) {
    return lookupVar(state->parent, tok);
  }

  printToken(tok);
  failEmit("Unknown variable!");
  return Value {};
}
