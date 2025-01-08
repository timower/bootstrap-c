import libc;

import ast;
import print_ast;
import util;

struct EmitState {
  tmpCounter : i32 *;
  vars : LocalVar *;

  curBreakLabel : i8 *;

  parent : EmitState *;
};

// LLVM IR Value
struct Value {
  type : const i8 *;

  // reg name or just the value
  val : const i8 *;
};

struct LocalVar {
  name : Token;
  value : Value;

  next : LocalVar *;
};

struct Case {
  val : Value;
  n : i32;

  next : Case *;
};

func failEmit(msg : const i8 *) {
  puts(msg);
  exit(1);
}

func failEmitLoc(loc : SourceLoc, msg : const i8 *) {
  printf("%s:%d:%d: %s\n", loc.fileName, loc.line, loc.column, msg);
  exit(1);
}

func failEmitExpr(expr : ExprAST *, msg : const i8 *) {
  failEmitLoc(expr->location, msg);
}

func newEmitState(parent : EmitState *) -> EmitState {
  let state : EmitState = {0};
  state.tmpCounter = parent->tmpCounter;
  state.parent = parent;
  state.curBreakLabel = parent->curBreakLabel;
  return state;
}

func getCount(state : EmitState *) -> i32 { return (*state->tmpCounter)++; }

// Convert type to LLVM type.
func convertType(type : Type *) -> const i8 * {
  switch (type->kind) {
  case TypeKind::VOID:
    return "void";

  case TypeKind::INT:
    let buf : i8 * = malloc(16);
    sprintf(buf, "i%d", type->size);
    return buf;

  case TypeKind::POINTER:
    return "ptr";

  case TypeKind::STRUCT:
    let len = type->tag.end - type->tag.data;
    let buf : i8 * = malloc((len + 10) as u64);
    sprintf(buf, "%%struct.%.*s", len, type->tag.data);
    return buf;

  case TypeKind::ARRAY:
    let buf : i8 * = malloc(32);
    sprintf(buf, "[%d x %s]", type->size, convertType(type->arg));
    return buf;

  case TypeKind::FUNC:
    let buf : i8 * = malloc(128);
    let cur = buf + sprintf(buf, "%s (", convertType(type->result));
    for (let arg = type->arg; arg != NULL; arg = arg->argNext) {
      cur += sprintf(cur, "%s", convertType(arg));
      if (arg->argNext != NULL) {
        cur += sprintf(cur, ", ");
      }
    }
    if (type->isVarargs) {
      cur += sprintf(cur, ", ...");
    }
    sprintf(cur, ")");
    return buf;

  case TypeKind::ENUM:
    return "i32";

  case TypeKind::TAG:
    failEmit("Unknown type to emit");
  }

  return NULL;
}

func newLocal(name : Token, val : Value) -> LocalVar * {
  let local : LocalVar * = calloc(1, sizeof(struct LocalVar));
  local->name = name;
  local->value = val;
  return local;
}

func intToVal(num : i32, type : Type *) -> Value {
  let val : Value;
  val.type = convertType(type);

  let buf : i8 * = malloc(16);
  sprintf(buf, "%d", num);
  val.val = buf;

  return val;
}

func getNextTemp(state : EmitState *) -> Value {
  let val : Value;

  let buf : i8 * = malloc(16);
  sprintf(buf, "%%tmp%d", getCount(state));
  val.val = buf;

  return val;
}

func getGlobal(ident : Token) -> Value {
  let val : Value;

  let len = ident.end - ident.data;
  let buf : i8 * = malloc((len + 2) as u64);
  sprintf(buf, "@%.*s", len, ident.data);
  val.val = buf;

  return val;
}

func getTempGlobal(state : EmitState *, prefix : const i8 *) -> Value {
  let val : Value;

  let buf : i8 * = malloc(64);
  sprintf(buf, "@%s%d", prefix, getCount(state));
  val.val = buf;

  return val;
}

func emitExpr(state : EmitState *, expr : ExprAST *) -> Value;

// Turns an i1 into an i32
func upcasti1(state : EmitState *, val : Value) -> Value {
  let up = getNextTemp(state);
  up.type = val.type;
  if (strcmp(up.type, "ptr") == 0) {
    failEmit("bool to pointer?");
  }
  printf("  %s = zext i1 %s to %s\n", up.val, val.val, up.type);
  return up;
}

// Turns an i32 into an i1
func makeBool(state : EmitState *, val : Value) -> Value {
  let up = getNextTemp(state);
  up.type = val.type;
  printf("  %s = icmp ne %s %s, 0\n", up.val, val.type, val.val);
  return up;
}

func lookupVar(state : EmitState *, tok : Token) -> Value {
  for (let local = state->vars; local != NULL; local = local->next) {
    if (tokCmp(tok, local->name)) {
      return local->value;
    }
  }

  if (state->parent != NULL) {
    return lookupVar(state->parent, tok);
  }

  printToken(tok);
  failEmit("Unkown variable!");
  let v : Value;
  v.val = "undef";
  return v;
}

func emitAddr(state : EmitState *, expr : ExprAST *) -> Value {
  switch (expr->kind) {
  case ExprKind::VARIABLE:
    return lookupVar(state, expr->identifier);

  case ExprKind::INDEX:
    let array : Value;
    if (expr->lhs->type->kind == TypeKind::ARRAY) {
      array = emitAddr(state, expr->lhs);
    } else {
      // array = emitExpr(state, expr->lhs);
      failEmitExpr(expr, "Unsupported index on non array");
    }
    let index = emitExpr(state, expr->rhs);

    let gep = getNextTemp(state);
    gep.type = "ptr";

    printf("  %s = getelementptr inbounds %s, ptr %s, %s %s\n", gep.val,
           convertType(expr->lhs->type->arg), array.val, index.type, index.val);
    return gep;

  case ExprKind::MEMBER:
    let agg = expr->op.kind == TokenKind::DOT ? emitAddr(state, expr->lhs)
                                              : emitExpr(state, expr->lhs);
    let aggType = expr->op.kind == TokenKind::DOT ? expr->lhs->type
                                                  : expr->lhs->type->arg;

    let gep = getNextTemp(state);
    gep.type = "ptr";
    printf("  %s = getelementptr inbounds %s, ptr %s, i32 0, i32 %d\n", gep.val,
           convertType(aggType), agg.val, expr->value);
    return gep;

  case ExprKind::UNARY:
    if (expr->op.kind == TokenKind::STAR) {
      return emitExpr(state, expr->rhs);
    }
  case ExprKind::PAREN:
    return emitAddr(state, expr->lhs);
  default:
    break;
  }

  failEmitExpr(expr, " Can't be use as lvalue");

  let v : Value;
  v.val = "undef";
  return v;
}

func emitStore(addr : Value, val : Value) {
  printf("  store %s %s, ptr %s\n", val.type, val.val, addr.val);
}

func emitLoad(state : EmitState *, addr : Value, type : const i8 *) -> Value {
  let val = getNextTemp(state);
  val.type = type;
  printf("  %s = load %s, ptr %s\n", val.val, val.type, addr.val);
  return val;
}

func emitBinary(state
                : EmitState *, resType
                : Type *, opKind
                : TokenKind, lhs
                : Value, lhsType
                : Type *, rhs
                : Value, rhsType
                : Type *) -> Value {

  // ptr - ptr -> i32
  let lhsPointer = lhsType->kind == TypeKind::POINTER;
  let rhsPointer = rhsType->kind == TypeKind::POINTER;
  if (lhsPointer && rhsPointer && opKind == TokenKind::MINUS) {
    failEmit("TODO");
  }

  if (lhsPointer != rhsPointer) {
    let ptrType = lhsPointer ? lhsType : rhsType;
    let ptrOp = lhsPointer ? lhs : rhs;
    let intOp = lhsPointer ? rhs : lhs;

    // negate the i32 for minus op
    if (opKind == TokenKind::MINUS) {
      let neg = getNextTemp(state);
      neg.type = intOp.type;
      printf("  %s = sub %s 0, %s\n", neg.val, neg.type, intOp.val);
      intOp = neg;
    }

    let res = getNextTemp(state);
    res.type = convertType(resType);
    printf("  %s = getelementptr inbounds %s, ptr %s, %s %s\n", res.val,
           convertType(ptrType->arg), ptrOp.val, intOp.type, intOp.val);
    return res;
  }

  if (strcmp(lhs.type, rhs.type) != 0) {
    printf("%s <> %s ", lhs.type, rhs.type);
    failEmit("Lhs and rhs don't have same type!");
  }

  let instr : const i8 *;
  let upcast = 0;
  switch (opKind) {
  default:
    failEmit("Invalid binary op");
  case TokenKind::PLUS:
    instr = "add";
  case TokenKind::MINUS:
    instr = "sub";
  case TokenKind::STAR:
    instr = "mul";
  case TokenKind::SLASH:
    instr = "sdiv";
  case TokenKind::PERCENT:
    instr = "srem";
  case TokenKind::LEFT_OP:
    instr = "shl";
  case TokenKind::RIGHT_OP:
    instr = "ashr";
  case TokenKind::LESS:
    instr = "icmp slt";
    upcast = 1;
  case TokenKind::GREATER:
    instr = "icmp sgt";
    upcast = 1;
  case TokenKind::LE_OP:
    instr = "icmp sle";
    upcast = 1;
  case TokenKind::GE_OP:
    instr = "icmp sge";
    upcast = 1;
  case TokenKind::EQ_OP:
    instr = "icmp eq";
    upcast = 1;
  case TokenKind::NE_OP:
    instr = "icmp ne";
    upcast = 1;
  case TokenKind::AND:
    instr = "and";
  case TokenKind::HAT:
    instr = "xor";
  case TokenKind::PIPE:
    instr = "or";
  }
  let res = getNextTemp(state);
  res.type = convertType(resType);
  printf("  %s = %s %s %s, %s\n", res.val, instr, lhs.type, lhs.val, rhs.val);

  if (upcast) {
    return upcasti1(state, res);
  }

  return res;
}

func emitAssignment(state : EmitState *, expr : ExprAST *) -> Value {
  let addr = emitAddr(state, expr->lhs);
  let val = emitExpr(state, expr->rhs);

  if (expr->op.kind == TokenKind::EQ) {
    emitStore(addr, val);
    return val;
  }

  let lval = emitLoad(state, addr, convertType(expr->lhs->type));

  let op : TokenKind;
  switch (expr->op.kind) {
  case TokenKind::ADD_ASSIGN:
    op = TokenKind::PLUS;
  case TokenKind::SUB_ASSIGN:
    op = TokenKind::MINUS;
  case TokenKind::MUL_ASSIGN:
    op = TokenKind::STAR;
  case TokenKind::DIV_ASSIGN:
    op = TokenKind::SLASH;
  case TokenKind::MOD_ASSIGN:
    op = TokenKind::PERCENT;
  case TokenKind::LEFT_ASSIGN:
    op = TokenKind::LEFT_OP;
  case TokenKind::RIGHT_ASSIGN:
    op = TokenKind::RIGHT_OP;
  case TokenKind::AND_ASSIGN:
    op = TokenKind::AND;
  case TokenKind::XOR_ASSIGN:
    op = TokenKind::HAT;
  case TokenKind::OR_ASSIGN:
    op = TokenKind::PIPE;
  default:
    failEmitExpr(expr, "Invalid assign op");
  }

  let res = emitBinary(state, expr->type, op, lval, expr->lhs->type, val,
                       expr->rhs->type);

  emitStore(addr, res);
  return res;
}

func emitLogicalBinOp(state : EmitState *, expr : ExprAST *) -> Value {
  let lhs = emitExpr(state, expr->lhs);
  let idx = getCount(state);

  let firstLabel : i8 * = "true";
  let secondLabel : i8 * = "false";
  if (expr->op.kind == TokenKind::OR_OP) {
    firstLabel = "false";
    secondLabel = "true";
  }

  let firstCmp = getNextTemp(state);
  printf("  br label %%entry.%d\n", idx);
  printf("entry.%d:\n", idx);
  printf("  %s = icmp ne %s %s, 0\n", firstCmp.val, lhs.type, lhs.val);
  printf("  br i1 %s, label %%%s.%d, label %%%s.%d\n", firstCmp.val, firstLabel,
         idx, secondLabel, idx);

  printf("true.%d:\n", idx);
  let rhs = emitExpr(state, expr->rhs);
  let secondCmp = getNextTemp(state);
  printf("  br label %%true.cont.%d\n", idx);
  printf("true.cont.%d:\n", idx);
  printf("  %s = icmp ne %s %s, 0\n", secondCmp.val, rhs.type, rhs.val);
  printf("  br label %%false.%d\n", idx);

  printf("false.%d:\n", idx);
  let res = getNextTemp(state);
  res.type = convertType(expr->type);
  printf("  %s = phi i1 [ %s, %%entry.%d ], [ %s, %%true.cont.%d ]\n", res.val,
         secondLabel, idx, secondCmp.val, idx);

  return upcasti1(state, res);
}

func emitPtrBinOp(state : EmitState *, expr : ExprAST *) -> Value {
  if (expr->lhs->type->kind == TypeKind::POINTER &&
      expr->rhs->type->kind == TypeKind::POINTER &&
      expr->op.kind == TokenKind::MINUS) {
    let lhs = emitExpr(state, expr->lhs);
    let rhs = emitExpr(state, expr->rhs);
    // ptrtoint
    let lhsInt = getNextTemp(state);
    lhsInt.type = convertType(expr->type);
    printf("  %s = ptrtoint %s %s to %s\n", lhsInt.val, lhs.type, lhs.val,
           lhsInt.type);
    // ptrtoint
    let rhsInt = getNextTemp(state);
    rhsInt.type = lhsInt.type;
    printf("  %s = ptrtoint %s %s to %s\n", rhsInt.val, rhs.type, rhs.val,
           rhsInt.type);

    // sub
    let res = getNextTemp(state);
    res.type = lhsInt.type;
    printf("  %s = sub %s %s, %s\n", res.val, res.type, lhsInt.val, rhsInt.val);

    return res;
  }

  let ptrExpr =
      expr->lhs->type->kind == TypeKind::POINTER ? expr->lhs : expr->rhs;
  let intExpr =
      expr->lhs->type->kind == TypeKind::POINTER ? expr->rhs : expr->lhs;

  let ptr = emitExpr(state, ptrExpr);
  let num = emitExpr(state, intExpr);

  // negate num
  if (expr->op.kind == TokenKind::MINUS) {
    let neg = getNextTemp(state);
    neg.type = num.type;
    printf("  %s = sub %s 0, %s\n", neg.val, num.type, num.val);
    num = neg;
  }

  let res = getNextTemp(state);
  res.type = "ptr";
  printf("  %s = getelementptr inbounds %s, %s %s, %s %s\n", res.val,
         convertType(ptrExpr->type->arg), ptr.type, ptr.val, num.type, num.val);
  return res;
}

func emitBinOp(state : EmitState *, expr : ExprAST *) -> Value {
  if (isAssign(expr->op)) {
    return emitAssignment(state, expr);
  }
  if (expr->op.kind == TokenKind::AND_OP || expr->op.kind == TokenKind::OR_OP) {
    return emitLogicalBinOp(state, expr);
  }

  // TODO: not needed anymore?
  if ((expr->op.kind == TokenKind::PLUS || expr->op.kind == TokenKind::MINUS ||
       expr->op.kind == TokenKind::ADD_ASSIGN ||
       expr->op.kind == TokenKind::SUB_ASSIGN) &&
      (expr->lhs->type->kind == TypeKind::POINTER ||
       expr->rhs->type->kind == TypeKind::POINTER)) {
    return emitPtrBinOp(state, expr);
  }

  let lhs = emitExpr(state, expr->lhs);
  let rhs = emitExpr(state, expr->rhs);

  if (expr->op.kind == TokenKind::COMMA) {
    return rhs;
  }

  return emitBinary(state, expr->type, expr->op.kind, lhs, expr->lhs->type, rhs,
                    expr->rhs->type);
}

func emitUnary(state : EmitState *, expr : ExprAST *) -> Value {
  if (expr->op.kind == TokenKind::AND) {
    let res = emitAddr(state, expr->rhs);
    res.type = "ptr";
    return res;
  }

  if (expr->op.kind == TokenKind::INC_OP ||
      expr->op.kind == TokenKind::DEC_OP) {
    let opExpr = expr->lhs == NULL ? expr->rhs : expr->lhs;
    let operand = emitAddr(state, opExpr);

    let val = emitLoad(state, operand, convertType(opExpr->type));

    // Use emitBinary to handle the inc/dec
    let type =
        opExpr->type->kind == TypeKind::POINTER ? getInt32() : opExpr->type;
    let one = intToVal(expr->op.kind == TokenKind::INC_OP ? 1 : -1, type);
    let res = emitBinary(state, opExpr->type, TokenKind::PLUS, val,
                         opExpr->type, one, type);
    emitStore(operand, res);

    if (expr->lhs != NULL) {
      return val;
    }
    return res;
  }

  let operand = emitExpr(state, expr->rhs);
  let res = getNextTemp(state);

  let instr : const i8 *;
  let constop : const i8 *;
  let upcast = 0;
  switch (expr->op.kind) {
  case TokenKind::PLUS:
    return operand;

  case TokenKind::STAR:
    res.type = convertType(expr->type);
    printf("  %s = load %s, ptr %s\n", res.val, res.type, operand.val);
    return res;

  case TokenKind::MINUS:
    instr = "sub";
    constop = "0";
  case TokenKind::TILDE:
    instr = "xor";
    constop = "-1";
  case TokenKind::BANG:
    instr = "icmp eq";
    constop = "0";
    upcast = 1;

  default:
    failEmitExpr(expr, "Invalid unary");
  }

  res.type = operand.type;
  printf("  %s = %s %s %s, %s\n", res.val, instr, operand.type, constop,
         operand.val);

  if (upcast) {
    return upcasti1(state, res);
  }

  return res;
}

func emitVarRef(state : EmitState *, expr : ExprAST *) -> Value {
  let addr = emitAddr(state, expr);

  // Funcs and arrays are implictly converted to pointers here.
  if (expr->type->kind == TypeKind::FUNC ||
      expr->type->kind == TypeKind::ARRAY) {
    addr.type = "ptr";
    return addr;
  }

  let val = getNextTemp(state);
  val.type = convertType(expr->type);
  printf("  %s = load %s, ptr %s\n", val.val, val.type, addr.val);
  return val;
}

func getStrConst(type : Type *, tok : Token) -> Value {
  let len = tok.end - tok.data;
  let val : i8 * = malloc((len + 16) as u64);

  let cur = val;
  cur += sprintf(val, "c\""); // %.*s\\00\"", len, tok.data);

  for (let i : i64 = 0; i < len; i++) {
    let val = *(tok.data + i);
    if (val == '\\') {
      let c = getEscaped(*(tok.data + ++i));
      cur += sprintf(cur, "\\%02x", c);
    } else {
      *cur++ = val;
    }
  }
  cur += sprintf(cur, "\\00\"");

  let res : Value;
  res.type = convertType(type);
  res.val = val;
  return res;
}

func emitStrRef(state : EmitState *, expr : ExprAST *) -> Value {

  let strGlobal = getTempGlobal(state, "str.");
  strGlobal.type = "ptr";

  let strType = newType(TypeKind::ARRAY);
  strType->arg = expr->type->arg;
  strType->size = (expr->identifier.end - expr->identifier.data + 1) as i32;
  let strConst = getStrConst(strType, expr->identifier);

  printf("%s = constant %s %s\n", strGlobal.val, strConst.type, strConst.val);

  return strGlobal;
}

func emitArray(state : EmitState *, expr : ExprAST *) -> Value {
  let res : Value;
  res.type = convertType(expr->type);
  if (expr->type->kind == TypeKind::STRUCT) {
    res.val = "zeroinitializer";
    return res;
  }

  let buf : i8 * = malloc(64 * expr->type->size as u64);
  res.val = buf;

  buf += sprintf(buf, "[ ");

  for (let field = expr; field != NULL; field = field->rhs) {
    let elem = emitExpr(state, field->lhs);
    buf += sprintf(buf, "%s %s", elem.type, elem.val);
    if (field->rhs != NULL) {
      buf += sprintf(buf, ", ");
    }
  }
  buf += sprintf(buf, " ]");

  return res;
}

func emitCall(state : EmitState *, expr : ExprAST *) -> Value {
  let args : LocalVar * = NULL;
  let argsTail : LocalVar * = NULL;

  for (let arg = expr->rhs; arg != NULL; arg = arg->rhs) {
    let eof : Token;
    let argVal = emitExpr(state, arg->lhs);

    let nextArg = newLocal(eof, argVal);
    if (args == NULL) {
      args = nextArg;
    } else {
      argsTail->next = nextArg;
    }
    argsTail = nextArg;
  }
  let fn = emitExpr(state, expr->lhs);

  let res : Value;
  if (expr->type->kind != TypeKind::VOID) {
    res = getNextTemp(state);
    printf("  %s = ", res.val);
  } else {
    res.val = "undef";
    printf("  ");
  }

  res.type = convertType(expr->type);
  printf("call %s %s(", convertType(expr->lhs->type), fn.val);
  for (; args != NULL; args = args->next) {
    printf("%s %s", args->value.type, args->value.val);
    if (args->next != NULL) {
      printf(", ");
    }
  }

  printf(")\n");

  return res;
}

func emitMemberOrIndex(state : EmitState *, expr : ExprAST *) -> Value {
  let addr = emitAddr(state, expr);
  return emitLoad(state, addr, convertType(expr->type));
}

func emitCast(state : EmitState *, expr : ExprAST *) -> Value {
  let v = emitExpr(state, expr->lhs);

  let from = expr->lhs->type;
  let to = expr->type;
  if (from->kind == TypeKind::POINTER && to->kind == TypeKind::POINTER) {
    return v;
  }

  if (from->kind == TypeKind::ENUM && to->kind == TypeKind::INT) {
    // Enum is i32
    if (to->size == 4 && to->isSigned) {
      return v;
    }
    from = getInt32();
  }

  if (from->kind == TypeKind::INT && to->kind == TypeKind::ENUM) {
    // Enum is i32
    if (from->size == 4 && from->isSigned) {
      return v;
    }
    to = getInt32();
  }

  if (from->kind != TypeKind::INT || to->kind != TypeKind::INT) {
    failEmitExpr(expr, "Unsupported cast");
  }

  // No-op, same size cast.
  if (from->size == to->size) {
    return v;
  }

  let res = getNextTemp(state);
  res.type = convertType(to);

  if (from->size > to->size) {
    printf("  %s = trunc %s %s to %s\n", res.val, v.type, v.val, res.type);
  } else if (from->size < to->size) {
    if (to->isSigned) {
      printf("  %s = sext %s %s to %s\n", res.val, v.type, v.val, res.type);
    } else {
      printf("  %s = zext %s %s to %s\n", res.val, v.type, v.val, res.type);
    }
  } else {
    // Is impossible due to the check above.
    failEmitExpr(expr, "Unsupported cast");
  }

  return res;
}

func emitCond(state : EmitState *, expr : ExprAST *) -> Value {
  let cond = emitExpr(state, expr->cond);
  cond = makeBool(state, cond);

  let falseLabel : i8 * = "false";
  let idx = getCount(state);
  printf("  br i1 %s, label %%cond.true.%d, label %%cond.%s.%d\n", cond.val,
         idx, falseLabel, idx);

  printf("cond.true.%d:\n", idx);
  let trueVal = emitExpr(state, expr->lhs);
  printf("  br label %%cond.cont.%d\n", idx);

  printf("cond.false.%d:\n", idx);
  let falseVal = emitExpr(state, expr->rhs);
  printf("  br label %%cond.cont.%d\n", idx);

  printf("cond.cont.%d:\n", idx);

  let res = getNextTemp(state);
  res.type = trueVal.type;
  printf("  %s = phi %s [ %s, %%cond.true.%d ], [ %s, %%cond.false.%d ]\n",
         res.val, res.type, trueVal.val, idx, falseVal.val, idx);
  return res;
}

func emitStructExpr(state : EmitState *, expr : ExprAST *) -> Value {
  let type = convertType(expr->type);

  let res : Value = {0};
  res.type = type;
  res.val = "zeroinitializer";

  for (let field = expr->rhs; field != NULL; field = field->rhs) {
    let fieldVal = emitExpr(state, field->lhs);
    let next = getNextTemp(state);
    next.type = type;
    printf("  %s = insertvalue %s %s, %s %s, %d\n", next.val, type, res.val,
           fieldVal.type, fieldVal.val, field->value);

    res = next;
  }

  return res;
}

// \returns The register name or value of the expr
func emitExpr(state : EmitState *, expr : ExprAST *) -> Value {
  switch (expr->kind) {
  case ExprKind::INT:
    if (expr->type->kind == TypeKind::POINTER) {
      if (expr->value != 0) {
        failEmitExpr(expr, "Only null ptr supported");
      }
      let v : Value;
      v.type = "ptr";
      v.val = "null";
      return v;
    }
    return intToVal(expr->value, expr->type);
  case ExprKind::SCOPE:
    return intToVal(expr->value, expr->type);
  case ExprKind::BINARY:
    return emitBinOp(state, expr);
  case ExprKind::UNARY:
    return emitUnary(state, expr);
  case ExprKind::VARIABLE:
    return emitVarRef(state, expr);

  case ExprKind::STR:
    if (expr->type->kind == TypeKind::ARRAY) {
      return getStrConst(expr->type, expr->identifier);
    }
    return emitStrRef(state, expr);

  case ExprKind::ARRAY:
    return emitArray(state, expr);

  case ExprKind::CALL:
    return emitCall(state, expr);

  case ExprKind::INDEX, ExprKind::MEMBER:
    return emitMemberOrIndex(state, expr);

  case ExprKind::CAST:
    return emitCast(state, expr);

  case ExprKind::CONDITIONAL:
    return emitCond(state, expr);

  case ExprKind::STRUCT:
    return emitStructExpr(state, expr);
  case ExprKind::PAREN:
    return emitExpr(state, expr->lhs);

  default:
    failEmitExpr(expr, "Unsupported expr");
  }

  let v : Value;
  v.type = "i32";
  v.val = "undef";
  return v;
}

func emitReturn(state : EmitState *, stmt : StmtAST *) {
  if (stmt->expr != NULL) {
    let v = emitExpr(state, stmt->expr);
    if (stmt->expr->type->kind != TypeKind::VOID) {
      printf("  ret %s %s\n", v.type, v.val);
      return;
    }
  }

  printf("  ret void\n");
}

func addLocal(state : EmitState *, name : Token, val : Value) {
  let local = newLocal(name, val);
  local->next = state->vars;
  state->vars = local;
}

func emitLocalVar(state : EmitState *, decl : DeclAST *) -> Value {
  let val = getNextTemp(state);
  val.type = "ptr";

  let type = convertType(decl->type);
  printf("  %s = alloca %s\n", val.val, type);

  let local = newLocal(decl->name, val);
  local->next = state->vars;
  state->vars = local;

  if (decl->init != NULL) {
    let init = emitExpr(state, decl->init);
    emitStore(val, init);
  }

  return val;
}

func emitLocalDecl(state : EmitState *, decl : DeclAST *) {
  switch (decl->kind) {
  case DeclKind::VAR:
    emitLocalVar(state, decl);

  default:
    failEmit("Local Unsupported");
  }
}

func emitStmt(state : EmitState *, stmt : StmtAST *);

func emitIf(state : EmitState *, stmt : StmtAST *) {
  let cond = emitExpr(state, stmt->expr);
  cond = makeBool(state, cond);

  let falseLabel : i8 * = "false";
  if (stmt->stmt == NULL) {
    falseLabel = "cont";
  }
  let idx = getCount(state);
  printf("  br i1 %s, label %%if.true.%d, label %%if.%s.%d\n", cond.val, idx,
         falseLabel, idx);

  printf("if.true.%d:\n", idx);
  emitStmt(state, stmt->init);
  printf("  br label %%if.cont.%d\n", idx);

  if (stmt->stmt != NULL) {
    printf("if.false.%d:\n", idx);
    emitStmt(state, stmt->stmt);
    printf("  br label %%if.cont.%d\n", idx);
  }

  printf("if.cont.%d:\n", idx);
}

func emitWhile(state : EmitState *, stmt : StmtAST *) {
  let idx = getCount(state);

  printf("  br label %%while.cond.%d\n", idx);
  printf("while.cond.%d:\n", idx);
  let cond = makeBool(state, emitExpr(state, stmt->expr));
  printf("  br i1 %s, label %%while.body.%d, label %%while.cont.%d\n", cond.val,
         idx, idx);

  let whileState = newEmitState(state);

  let buf : i8 * = malloc(32);
  sprintf(buf, "while.cont.%d", idx);
  whileState.curBreakLabel = buf;

  printf("while.body.%d:\n", idx);
  emitStmt(&whileState, stmt->stmt);
  printf(" br label %%while.cond.%d\n", idx);

  printf("while.cont.%d:\n", idx);
}

func emitFor(state : EmitState *, stmt : StmtAST *) {
  let idx = getCount(state);

  let forState = newEmitState(state);

  let buf : i8 * = malloc(32);
  sprintf(buf, "for.cont.%d", idx);
  forState.curBreakLabel = buf;

  emitStmt(&forState, stmt->init);

  printf("  br label %%for.cond.%d\n", idx);
  printf("for.cond.%d:\n", idx);
  // cond must be an expression stmt, parseFor guarantees it.
  let cond = makeBool(&forState, emitExpr(&forState, stmt->cond->expr));
  printf("  br i1 %s, label %%for.body.%d, label %%for.cont.%d\n", cond.val,
         idx, idx);

  printf("for.body.%d:\n", idx);
  emitStmt(&forState, stmt->stmt);
  printf("  br label %%for.incr.%d\n", idx);

  // TODO: continue would jump here
  printf("for.incr.%d:\n", idx);
  emitExpr(&forState, stmt->expr);
  printf("  br label %%for.cond.%d\n", idx);

  printf("for.cont.%d:\n", idx);
}

func getCases(state
              : EmitState *, expr
              : ExprAST *, cases
              : Case *, index
              : i32) -> Case * {
  switch (expr->kind) {
  case ExprKind::SCOPE, ExprKind::INT:
    let constExpr = emitExpr(state, expr);
    let cse : Case * = calloc(1, sizeof(struct Case));
    cse->n = index;
    cse->val = constExpr;
    cse->next = cases;
    return cse;
  case ExprKind::BINARY:
    let lhsCases = getCases(state, expr->lhs, cases, index);
    return getCases(state, expr->rhs, lhsCases, index);
  default:
    failEmitExpr(expr, "Unsupported case expr");
  }
}

func emitSwitch(state : EmitState *, stmt : StmtAST *) {
  let switchIdx = getCount(state);

  let switchState = newEmitState(state);

  let buf : i8 * = malloc(32);
  sprintf(buf, "cont.%d", switchIdx);
  switchState.curBreakLabel = buf;

  let expr = emitExpr(&switchState, stmt->expr);
  printf("  br label %%switch.%d\n", switchIdx);

  let cases : Case * = NULL;
  let defaultLabel : i8 * = NULL;

  for (let caseStmt = stmt->stmt; caseStmt != NULL;
       caseStmt = caseStmt->nextStmt) {

    // Jump to end of switch at the end of the previous case.
    printf("  br label %%cont.%d\n", switchIdx);

    if (caseStmt->kind == StmtKind::CASE) {
      let caseIdx = getCount(state);

      printf("case.%d:\n", caseIdx);

      cases = getCases(state, caseStmt->expr, cases, caseIdx);

    } else if (caseStmt->kind == StmtKind::DEFAULT) {

      if (defaultLabel != NULL) {
        failEmit("Multiple default");
      }

      let defaultIdx = getCount(state);
      defaultLabel = malloc(32);
      sprintf(defaultLabel, "default.%d", defaultIdx);

      printf("default.%d:\n", defaultIdx);

    } else {
      failEmit("Unsupported switch stmt");
    }

    let caseState = newEmitState(&switchState);
    for (let cur = caseStmt->stmt; cur != NULL; cur = cur->nextStmt) {
      emitStmt(&caseState, cur);
    }
  }

  // fallthrough to the end of the switch
  printf("  br label %%cont.%d\n", switchIdx);

  printf("switch.%d:\n", switchIdx);

  if (defaultLabel != NULL) {
    printf("  switch %s %s, label %%%s [\n", expr.type, expr.val, defaultLabel);
  } else {
    printf("  switch %s %s, label %%cont.%d [\n", expr.type, expr.val,
           switchIdx);
  }
  for (let cse = cases; cse != NULL; cse = cse->next) {
    printf("    %s %s, label %%case.%d\n", cse->val.type, cse->val.val, cse->n);
  }
  printf("  ]\n");
  printf("cont.%d:\n", switchIdx);
}

func emitStmt(state : EmitState *, stmt : StmtAST *) {
  switch (stmt->kind) {
  case StmtKind::EXPR:
    if (stmt->expr != NULL) {
      emitExpr(state, stmt->expr);
    }
  case StmtKind::RETURN:
    emitReturn(state, stmt);
  case StmtKind::DECL:
    emitLocalDecl(state, stmt->decl);
  case StmtKind::COMPOUND:
    let newState = newEmitState(state);
    for (let cur = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      emitStmt(&newState, cur);
    }
  case StmtKind::IF:
    return emitIf(state, stmt);

  case StmtKind::WHILE:
    return emitWhile(state, stmt);
  case StmtKind::FOR:
    return emitFor(state, stmt);

  case StmtKind::SWITCH:
    return emitSwitch(state, stmt);

  case StmtKind::CASE, StmtKind::DEFAULT:
    failEmit("Case outside of switch");
  case StmtKind::BREAK:
    if (state->curBreakLabel == NULL) {
      failEmit("Break outside loop");
    }
    printf("  br label %%%s\n", state->curBreakLabel);
  }
}

func emitFunc(state : EmitState *, decl : DeclAST *) {
  let val = getGlobal(decl->name);
  val.type = "ptr";
  addLocal(state, decl->name, val);

  if (decl->hasDef && decl->body == NULL) {
    return;
  }

  let defOrDecl = decl->body == NULL ? "declare" as i8 * : "define" as i8 *;
  printf("%s %s %s(", defOrDecl, convertType(decl->type->result), val.val);
  for (let arg = decl->fields; arg != NULL; arg = arg->next) {
    let len = arg->name.end - arg->name.data;
    printf("%s %%%.*s", convertType(arg->type), len, arg->name.data);
    if (arg->next != NULL) {
      printf(", ");
    }
  }
  printf(")");

  if (decl->body != NULL) {
    let funcState = newEmitState(state);
    let funcCounter = 0;
    funcState.tmpCounter = &funcCounter;

    printf(" {\n");

    for (let arg = decl->fields; arg != NULL; arg = arg->next) {
      let addr = emitLocalVar(&funcState, arg);
      let len = arg->name.end - arg->name.data;
      printf("  store %s %%%.*s, ptr %s\n", convertType(arg->type), len,
             arg->name.data, addr.val);
    }
    emitStmt(&funcState, decl->body);

    // Emit implict void return.
    if (decl->type->result->kind == TypeKind::VOID) {
      printf("  ret void\n");
    } else {
      printf("  ret %s undef\n", convertType(decl->type->result));
    }

    printf("}\n");
  } else {
    printf("\n");
  }
}

func emitStruct(state : EmitState *, decl : DeclAST *) {
  // emit nested structs
  for (let field = decl->fields; field != NULL; field = field->next) {
    if (field->kind == DeclKind::STRUCT) {
      emitStruct(state, field);
    }
  }

  // TODO: padding
  printf("%s = type <{ ", convertType(decl->type));

  for (let field = decl->fields; field != NULL; field = field->next) {
    printf("%s", convertType(field->type));
    if (field->next != NULL) {
      printf(", ");
    }
  }

  printf(" }>\n");
}

func emitGlobalVar(state : EmitState *, decl : DeclAST *) {
  let declSpec = decl->type->isConst ? "constant" as i8 * : "global" as i8 *;

  let val = getGlobal(decl->name);
  val.type = convertType(decl->type);
  if (decl->init != NULL) {
    let init = emitExpr(state, decl->init); // TODO: emit constant
    printf("%s = %s %s %s\n", val.val, declSpec, init.type, init.val);
  } else {
    let init = decl->type->kind == TypeKind::STRUCT ? "zeroinitializer" as i8 *
                                                    : "null" as i8 *;
    printf("%s = %s %s %s\n", val.val, declSpec, val.type, init);
  }

  addLocal(state, decl->name, val);
}

func emitGlobalDecl(state : EmitState *, decl : DeclAST *) {
  switch (decl->kind) {
  case DeclKind::ENUM:
    return;
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

func emitTopLevel(decl : DeclAST *) {
  let state : EmitState = {0};
  let rootCounter = 0;
  state.tmpCounter = &rootCounter;

  while (decl != NULL) {
    emitGlobalDecl(&state, decl);
    decl = decl->next;
  }
}
