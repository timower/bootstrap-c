import libc;

import ast;
import util;

struct EmitState {
  i32 tmpCounter;
  struct LocalVar *vars;

  i8 *curBreakLabel;
  i8 *defaultLabel;
  struct Case *cases;

  struct EmitState *parent;
};

// LLVM IR Value
struct Value {
  const i8 *type;

  // reg name or just the value
  const i8 *val;
};

struct LocalVar {
  struct Token name;
  struct Value value;

  struct LocalVar *next;
};

struct Case {
  struct Value val;
  i32 n;

  struct Case *next;
};

// 3. emit
void failEmit(const i8 *msg) {
  puts(msg);
  exit(1);
}

// Convert type to LLVM type.
const i8 *convertType(struct Type *type) {
  switch (type->kind) {
  case TypeKind::VOID:
    return "void";
  case TypeKind::INT: {
    i8 *buf = malloc(16);
    sprintf(buf, "i%d", type->size);
    return buf;
  }
  case TypeKind::POINTER:
    return "ptr";
  case TypeKind::STRUCT: {
    i64 len = type->tag.end - type->tag.data;
    i8 *buf = malloc((len + 10) as u64);
    sprintf(buf, "%%struct.%.*s", len, type->tag.data);
    return buf;
  }

  case TypeKind::ARRAY: {
    i8 *buf = malloc(32);
    sprintf(buf, "[%d x %s]", type->size, convertType(type->arg));
    return buf;
  }

  case TypeKind::FUNC: {
    i8 *buf = malloc(128);
    i8 *cur = buf + sprintf(buf, "%s (", convertType(type->result));
    for (struct Type *arg = type->arg; arg != NULL; arg = arg->argNext) {
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
  }

  case TypeKind::ENUM:
    return "i32";
  }

  failEmit("Unknown type to emit");
  return NULL;
}

struct LocalVar *newLocal(struct Token name, struct Value val) {
  struct LocalVar *local = calloc(1, sizeof(struct LocalVar));
  local->name = name;
  local->value = val;
  return local;
}

struct Value intToVal(i32 num, struct Type *type) {
  struct Value val;
  val.type = convertType(type);

  i8 *buf = malloc(16);
  sprintf(buf, "%d", num);
  val.val = buf;

  return val;
}

struct Value getNextTemp(struct EmitState *state) {
  struct Value val;

  i8 *buf = malloc(16);
  sprintf(buf, "%%tmp%d", state->tmpCounter++);
  val.val = buf;

  return val;
}

struct Value getGlobal(struct Token ident) {
  struct Value val;

  i64 len = ident.end - ident.data;
  i8 *buf = malloc((len + 2) as u64);
  sprintf(buf, "@%.*s", len, ident.data);
  val.val = buf;

  return val;
}

struct Value getTempGlobal(struct EmitState *state, const i8 *prefix) {
  struct Value val;

  i8 *buf = malloc(64);
  sprintf(buf, "@%s%d", prefix, state->tmpCounter++);
  val.val = buf;

  return val;
}

struct Value emitExpr(struct EmitState *state, struct ExprAST *expr);

// Turns an i1 into an i32
struct Value upcasti1(struct EmitState *state, struct Value val) {
  struct Value up = getNextTemp(state);
  up.type = val.type;
  if (strcmp(up.type, "ptr") == 0) {
    failEmit("bool to pointer?");
  }
  printf("  %s = zext i1 %s to %s\n", up.val, val.val, up.type);
  return up;
}

// Turns an i32 into an i1
struct Value makeBool(struct EmitState *state, struct Value val) {
  struct Value up = getNextTemp(state);
  up.type = val.type;
  printf("  %s = icmp ne %s %s, 0\n", up.val, val.type, val.val);
  return up;
}

struct Value lookupVar(struct EmitState *state, struct Token tok) {
  for (struct LocalVar *local = state->vars; local != NULL;
       local = local->next) {
    if (tokCmp(tok, local->name)) {
      return local->value;
    }
  }

  if (state->parent != NULL) {
    return lookupVar(state->parent, tok);
  }

  printToken(tok);
  failEmit("Unkown variable!");
  struct Value v;
  v.val = "undef";
  return v;
}

struct Value emitAddr(struct EmitState *state, struct ExprAST *expr) {
  switch (expr->kind) {
  case ExprKind::VARIABLE:
    return lookupVar(state, expr->identifier);

  case ExprKind::INDEX: {
    struct Value array = emitExpr(state, expr->lhs);
    struct Value index = emitExpr(state, expr->rhs);

    struct Value gep = getNextTemp(state);
    gep.type = "ptr";

    printf("  %s = getelementptr inbounds %s, ptr %s, %s %s\n", gep.val,
           convertType(expr->lhs->type->arg), array.val, index.type, index.val);
    return gep;
  }

  case ExprKind::MEMBER: {
    struct Value agg = expr->op.kind == TokenKind::DOT
                           ? emitAddr(state, expr->lhs)
                           : emitExpr(state, expr->lhs);
    struct Type *aggType = expr->op.kind == TokenKind::DOT
                               ? expr->lhs->type
                               : expr->lhs->type->arg;

    struct Value gep = getNextTemp(state);
    gep.type = "ptr";
    printf("  %s = getelementptr inbounds %s, ptr %s, i32 0, i32 %d\n", gep.val,
           convertType(aggType), agg.val, expr->value);
    return gep;
  }

  case ExprKind::UNARY:
    if (expr->op.kind == TokenKind::STAR) {
      return emitExpr(state, expr->rhs);
    }
  case ExprKind::INT:
  case ExprKind::BINARY:
  case ExprKind::CONDITIONAL:
  case ExprKind::SIZEOF:
  case ExprKind::STR:
  case ExprKind::ARRAY:
  case ExprKind::CALL:
  case ExprKind::ARG_LIST:
  case ExprKind::CAST:
    printExpr(expr);
    failEmit(" Can't be use as lvalue");
    break;
  }

  struct Value v;
  v.val = "undef";
  return v;
}

void emitStore(struct Value addr, struct Value val) {
  printf("  store %s %s, ptr %s\n", val.type, val.val, addr.val);
}

struct Value emitLoad(struct EmitState *state, struct Value addr,
                      const i8 *type) {
  struct Value val = getNextTemp(state);
  val.type = type;
  printf("  %s = load %s, ptr %s\n", val.val, val.type, addr.val);
  return val;
}

struct Value emitBinary(struct EmitState *state, struct Type *resType,
                        enum TokenKind opKind, struct Value lhs,
                        struct Type *lhsType, struct Value rhs,
                        struct Type *rhsType) {

  // ptr - ptr -> i32
  i32 lhsPointer = lhsType->kind == TypeKind::POINTER;
  i32 rhsPointer = rhsType->kind == TypeKind::POINTER;
  if (lhsPointer && rhsPointer && opKind == TokenKind::MINUS) {
    failEmit("TODO");
  }

  if (lhsPointer != rhsPointer) {
    struct Type *ptrType = lhsPointer ? lhsType : rhsType;
    struct Value ptrOp = lhsPointer ? lhs : rhs;
    struct Value intOp = lhsPointer ? rhs : lhs;

    // negate the i32 for minus op
    if (opKind == TokenKind::MINUS) {
      struct Value neg = getNextTemp(state);
      neg.type = intOp.type;
      printf("  %s = sub %s 0, %s\n", neg.val, neg.type, intOp.val);
      intOp = neg;
    }

    struct Value res = getNextTemp(state);
    res.type = convertType(resType);
    printf("  %s = getelementptr inbounds %s, ptr %s, %s %s\n", res.val,
           convertType(ptrType->arg), ptrOp.val, intOp.type, intOp.val);
    return res;
  }

  if (strcmp(lhs.type, rhs.type) != 0) {
    printf("%s <> %s ", lhs.type, rhs.type);
    failEmit("Lhs and rhs don't have same type!");
  }

  const i8 *instr;
  i32 upcast = 0;
  switch (opKind) {
  default:
    failEmit("Invalid binary op");
    break;
  case TokenKind::PLUS:
    instr = "add";
    break;
  case TokenKind::MINUS:
    instr = "sub";
    break;
  case TokenKind::STAR:
    instr = "mul";
    break;
  case TokenKind::SLASH:
    instr = "sdiv";
    break;
  case TokenKind::PERCENT:
    instr = "srem";
    break;

  case TokenKind::LEFT_OP:
    instr = "shl";
    break;
  case TokenKind::RIGHT_OP:
    instr = "ashr";
    break;

  case TokenKind::LESS:
    instr = "icmp slt";
    upcast = 1;
    break;
  case TokenKind::GREATER:
    instr = "icmp sgt";
    upcast = 1;
    break;
  case TokenKind::LE_OP:
    instr = "icmp sle";
    upcast = 1;
    break;
  case TokenKind::GE_OP:
    instr = "icmp sge";
    upcast = 1;
    break;
  case TokenKind::EQ_OP:
    instr = "icmp eq";
    upcast = 1;
    break;
  case TokenKind::NE_OP:
    instr = "icmp ne";
    upcast = 1;
    break;

  case TokenKind::AND:
    instr = "and";
    break;
  case TokenKind::HAT:
    instr = "xor";
    break;
  case TokenKind::PIPE:
    instr = "or";
    break;
  }
  struct Value res = getNextTemp(state);
  res.type = convertType(resType);
  printf("  %s = %s %s %s, %s\n", res.val, instr, lhs.type, lhs.val, rhs.val);

  if (upcast) {
    return upcasti1(state, res);
  }

  return res;
}

struct Value emitAssignment(struct EmitState *state, struct ExprAST *expr) {
  struct Value addr = emitAddr(state, expr->lhs);
  struct Value val = emitExpr(state, expr->rhs);

  if (expr->op.kind == TokenKind::EQ) {
    emitStore(addr, val);
    return val;
  }

  struct Value lval = emitLoad(state, addr, convertType(expr->lhs->type));

  enum TokenKind op;
  switch (expr->op.kind) {
  case TokenKind::ADD_ASSIGN:
    op = TokenKind::PLUS;
    break;
  case TokenKind::SUB_ASSIGN:
    op = TokenKind::MINUS;
    break;
  case TokenKind::MUL_ASSIGN:
    op = TokenKind::STAR;
    break;
  case TokenKind::DIV_ASSIGN:
    op = TokenKind::SLASH;
    break;
  case TokenKind::MOD_ASSIGN:
    op = TokenKind::PERCENT;
    break;
  case TokenKind::LEFT_ASSIGN:
    op = TokenKind::LEFT_OP;
    break;
  case TokenKind::RIGHT_ASSIGN:
    op = TokenKind::RIGHT_OP;
    break;
  case TokenKind::AND_ASSIGN:
    op = TokenKind::AND;
    break;
  case TokenKind::XOR_ASSIGN:
    op = TokenKind::HAT;
    break;
  case TokenKind::OR_ASSIGN:
    op = TokenKind::PIPE;
    break;
  default:
    failEmit("Invalid assign op");
  }

  struct Value res = emitBinary(state, expr->type, op, lval, expr->lhs->type,
                                val, expr->rhs->type);

  emitStore(addr, res);
  return res;
}

struct Value emitLogicalBinOp(struct EmitState *state, struct ExprAST *expr) {
  struct Value lhs = emitExpr(state, expr->lhs);
  i32 idx = state->tmpCounter++;

  const i8 *firstLabel = "true";
  const i8 *secondLabel = "false";
  if (expr->op.kind == TokenKind::OR_OP) {
    firstLabel = "false";
    secondLabel = "true";
  }

  struct Value firstCmp = getNextTemp(state);
  printf("  br label %%entry.%d\n", idx);
  printf("entry.%d:\n", idx);
  printf("  %s = icmp ne %s %s, 0\n", firstCmp.val, lhs.type, lhs.val);
  printf("  br i1 %s, label %%%s.%d, label %%%s.%d\n", firstCmp.val, firstLabel,
         idx, secondLabel, idx);

  printf("true.%d:\n", idx);
  struct Value rhs = emitExpr(state, expr->rhs);
  struct Value secondCmp = getNextTemp(state);
  printf("  br label %%true.cont.%d\n", idx);
  printf("true.cont.%d:\n", idx);
  printf("  %s = icmp ne %s %s, 0\n", secondCmp.val, rhs.type, rhs.val);
  printf("  br label %%false.%d\n", idx);

  printf("false.%d:\n", idx);
  struct Value res = getNextTemp(state);
  res.type = convertType(expr->type);
  printf("  %s = phi i1 [ %s, %%entry.%d ], [ %s, %%true.cont.%d ]\n", res.val,
         secondLabel, idx, secondCmp.val, idx);

  return upcasti1(state, res);
}

struct Value emitPtrBinOp(struct EmitState *state, struct ExprAST *expr) {
  if (expr->lhs->type->kind == TypeKind::POINTER &&
      expr->rhs->type->kind == TypeKind::POINTER &&
      expr->op.kind == TokenKind::MINUS) {
    struct Value lhs = emitExpr(state, expr->lhs);
    struct Value rhs = emitExpr(state, expr->rhs);
    // ptrtoint
    struct Value lhsInt = getNextTemp(state);
    lhsInt.type = convertType(expr->type);
    printf("  %s = ptrtoint %s %s to %s\n", lhsInt.val, lhs.type, lhs.val,
           lhsInt.type);
    // ptrtoint
    struct Value rhsInt = getNextTemp(state);
    rhsInt.type = lhsInt.type;
    printf("  %s = ptrtoint %s %s to %s\n", rhsInt.val, rhs.type, rhs.val,
           rhsInt.type);

    // sub
    struct Value res = getNextTemp(state);
    res.type = lhsInt.type;
    printf("  %s = sub %s %s, %s\n", res.val, res.type, lhsInt.val, rhsInt.val);

    return res;
  }

  struct ExprAST *ptrExpr =
      expr->lhs->type->kind == TypeKind::POINTER ? expr->lhs : expr->rhs;
  struct ExprAST *intExpr =
      expr->lhs->type->kind == TypeKind::POINTER ? expr->rhs : expr->lhs;

  struct Value ptr = emitExpr(state, ptrExpr);
  struct Value num = emitExpr(state, intExpr);

  // negate num
  if (expr->op.kind == TokenKind::MINUS) {
    struct Value neg = getNextTemp(state);
    neg.type = num.type;
    printf("  %s = sub %s 0, %s\n", neg.val, num.type, num.val);
    num = neg;
  }

  struct Value res = getNextTemp(state);
  res.type = "ptr";
  printf("  %s = getelementptr inbounds %s, %s %s, %s %s\n", res.val,
         convertType(ptrExpr->type->arg), ptr.type, ptr.val, num.type, num.val);
  return res;
}

struct Value emitBinOp(struct EmitState *state, struct ExprAST *expr) {
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

  struct Value lhs = emitExpr(state, expr->lhs);
  struct Value rhs = emitExpr(state, expr->rhs);

  if (expr->op.kind == TokenKind::COMMA) {
    return rhs;
  }

  return emitBinary(state, expr->type, expr->op.kind, lhs, expr->lhs->type, rhs,
                    expr->rhs->type);
}

struct Value emitUnary(struct EmitState *state, struct ExprAST *expr) {
  if (expr->op.kind == TokenKind::AND) {
    struct Value res = emitAddr(state, expr->rhs);
    res.type = "ptr";
    return res;
  }

  if (expr->op.kind == TokenKind::INC_OP ||
      expr->op.kind == TokenKind::DEC_OP) {
    struct ExprAST *opExpr = expr->lhs == NULL ? expr->rhs : expr->lhs;
    struct Value operand = emitAddr(state, opExpr);

    struct Value val = emitLoad(state, operand, convertType(opExpr->type));

    // Use emitBinary to handle the inc/dec
    struct Type *type =
        opExpr->type->kind == TypeKind::POINTER ? getInt32() : opExpr->type;
    struct Value one =
        intToVal(expr->op.kind == TokenKind::INC_OP ? 1 : -1, type);
    struct Value res = emitBinary(state, opExpr->type, TokenKind::PLUS, val,
                                  opExpr->type, one, type);
    emitStore(operand, res);

    if (expr->lhs != NULL) {
      return val;
    }
    return res;
  }

  struct Value operand = emitExpr(state, expr->rhs);
  struct Value res = getNextTemp(state);

  const i8 *instr;
  const i8 *constop;
  i32 upcast = 0;
  switch (expr->op.kind) {
  default:
  case TokenKind::INC_OP:
  case TokenKind::DEC_OP:
    failEmit("Invalid unary");
    break;

  case TokenKind::STAR:
    res.type = convertType(expr->type);
    printf("  %s = load %s, ptr %s\n", res.val, res.type, operand.val);
    return res;

  case TokenKind::PLUS:
    return operand;
  case TokenKind::MINUS:
    instr = "sub";
    constop = "0";
    break;
  case TokenKind::TILDE:
    instr = "xor";
    constop = "-1";
    break;
  case TokenKind::BANG:
    instr = "icmp eq";
    constop = "0";
    upcast = 1;
    break;
  }

  res.type = operand.type;
  printf("  %s = %s %s %s, %s\n", res.val, instr, operand.type, constop,
         operand.val);

  if (upcast) {
    return upcasti1(state, res);
  }

  return res;
}

struct Value emitVarRef(struct EmitState *state, struct ExprAST *expr) {
  struct Value addr = emitAddr(state, expr);

  // Funcs and arrays are implictly converted to pointers here.
  if (expr->type->kind == TypeKind::FUNC ||
      expr->type->kind == TypeKind::ARRAY ||
      (expr->type->kind == TypeKind::POINTER && expr->type->isDecay)) {
    addr.type = "ptr";
    return addr;
  }

  struct Value val = getNextTemp(state);
  val.type = convertType(expr->type);
  printf("  %s = load %s, ptr %s\n", val.val, val.type, addr.val);
  return val;
}

struct Value getStrConst(struct Type *type, struct Token tok) {
  i64 len = tok.end - tok.data;
  i8 *val = malloc((len + 16) as u64);

  i8 *cur = val;
  cur += sprintf(val, "c\""); // %.*s\\00\"", len, tok.data);

  for (i64 i = 0; i < len; i++) {
    if (tok.data[i] == '\\') {
      i8 c = getEscaped(tok.data[++i]);
      cur += sprintf(cur, "\\%02x", c);
    } else {
      *cur++ = tok.data[i];
    }
  }
  cur += sprintf(cur, "\\00\"");

  struct Value res;
  res.type = convertType(type);
  res.val = val;
  return res;
}

struct Value emitStrRef(struct EmitState *state, struct ExprAST *expr) {

  struct Value strGlobal = getTempGlobal(state, "str.");
  strGlobal.type = "ptr";

  struct Type *strType = newType(TypeKind::ARRAY);
  strType->arg = expr->type->arg;
  strType->size = (expr->identifier.end - expr->identifier.data + 1) as i32;
  struct Value strConst = getStrConst(strType, expr->identifier);

  printf("%s = constant %s %s\n", strGlobal.val, strConst.type, strConst.val);

  return strGlobal;
}

struct Value emitArray(struct EmitState *state, struct ExprAST *expr) {
  struct Value res;
  res.type = convertType(expr->type);
  if (expr->type->kind == TypeKind::STRUCT) {
    res.val = "zeroinitializer";
    return res;
  }

  i8 *buf = malloc(64 * expr->type->size as u64);
  res.val = buf;

  buf += sprintf(buf, "[ ");

  for (struct ExprAST *field = expr; field != NULL; field = field->rhs) {
    struct Value elem = emitExpr(state, field->lhs);
    buf += sprintf(buf, "%s %s", elem.type, elem.val);
    if (field->rhs != NULL) {
      buf += sprintf(buf, ", ");
    }
  }
  buf += sprintf(buf, " ]");

  return res;
}

struct Value emitCall(struct EmitState *state, struct ExprAST *expr) {
  struct LocalVar *args = NULL;
  struct LocalVar *argsTail = NULL;

  for (struct ExprAST *arg = expr->rhs; arg != NULL; arg = arg->rhs) {
    struct Token eof;
    struct Value argVal = emitExpr(state, arg->lhs);

    struct LocalVar *nextArg = newLocal(eof, argVal);
    if (args == NULL) {
      args = nextArg;
    } else {
      argsTail->next = nextArg;
    }
    argsTail = nextArg;
  }
  struct Value fn = emitExpr(state, expr->lhs);

  struct Value res;
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

struct Value emitMemberOrIndex(struct EmitState *state, struct ExprAST *expr) {
  struct Value addr = emitAddr(state, expr);
  return emitLoad(state, addr, convertType(expr->type));
}

struct Value emitCast(struct EmitState *state, struct ExprAST *expr) {
  struct Value v = emitExpr(state, expr->lhs);

  struct Type *from = expr->lhs->type;
  struct Type *to = expr->type;
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
    failEmit("Unsupported cast");
  }

  // No-op, same size cast.
  if (from->size == to->size) {
    return v;
  }

  struct Value res = getNextTemp(state);
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
    failEmit("Unsupported cast");
  }

  return res;
}

struct Value emitCond(struct EmitState *state, struct ExprAST *expr) {
  struct Value cond = emitExpr(state, expr->cond);
  cond = makeBool(state, cond);

  const i8 *falseLabel = "false";
  i32 idx = state->tmpCounter++;
  printf("  br i1 %s, label %%cond.true.%d, label %%cond.%s.%d\n", cond.val,
         idx, falseLabel, idx);

  printf("cond.true.%d:\n", idx);
  struct Value trueVal = emitExpr(state, expr->lhs);
  printf("  br label %%cond.cont.%d\n", idx);

  printf("cond.false.%d:\n", idx);
  struct Value falseVal = emitExpr(state, expr->rhs);
  printf("  br label %%cond.cont.%d\n", idx);

  printf("cond.cont.%d:\n", idx);

  struct Value res = getNextTemp(state);
  res.type = trueVal.type;
  printf("  %s = phi %s [ %s, %%cond.true.%d ], [ %s, %%cond.false.%d ]\n",
         res.val, res.type, trueVal.val, idx, falseVal.val, idx);
  return res;
}
// \returns The register name or value of the expr
struct Value emitExpr(struct EmitState *state, struct ExprAST *expr) {
  switch (expr->kind) {
  case ExprKind::INT:
    if (expr->type->kind == TypeKind::POINTER) {
      if (expr->value != 0) {
        failEmit("Only null ptr supported");
      }
      struct Value v;
      v.type = "ptr";
      v.val = "null";
      return v;
    }
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

  case ExprKind::INDEX:
  case ExprKind::MEMBER:
    return emitMemberOrIndex(state, expr);

  case ExprKind::CAST:
    return emitCast(state, expr);

  case ExprKind::CONDITIONAL:
    return emitCond(state, expr);

  case ExprKind::SIZEOF:
    printExpr(expr);

  case ExprKind::ARG_LIST:
  default:
    failEmit("Unsupported expr");
  }

  struct Value v;
  v.type = "i32";
  v.val = "undef";
  return v;
}

void emitReturn(struct EmitState *state, struct StmtAST *stmt) {
  if (stmt->expr != NULL) {
    struct Value v = emitExpr(state, stmt->expr);
    if (stmt->expr->type->kind != TypeKind::VOID) {
      printf("  ret %s %s\n", v.type, v.val);
      return;
    }
  }

  printf("  ret void\n");
}

void addLocal(struct EmitState *state, struct Token name, struct Value val) {
  struct LocalVar *local = newLocal(name, val);
  local->next = state->vars;
  state->vars = local;
}

struct Value emitLocalVar(struct EmitState *state, struct DeclAST *decl) {
  struct Value val = getNextTemp(state);
  val.type = "ptr";

  const i8 *type = convertType(decl->type);
  printf("  %s = alloca %s\n", val.val, type);

  struct LocalVar *local = newLocal(decl->name, val);
  local->next = state->vars;
  state->vars = local;

  if (decl->init != NULL) {
    struct Value init = emitExpr(state, decl->init);
    emitStore(val, init);
  }

  return val;
}

void emitLocalDecl(struct EmitState *state, struct DeclAST *decl) {
  switch (decl->kind) {
  case DeclKind::VAR:
    emitLocalVar(state, decl);
    break;

  case DeclKind::ENUM:
  case DeclKind::ENUM_FIELD:
  case DeclKind::STRUCT:
  case DeclKind::FUNC:
    failEmit("Local Unsupported");
  }
}

void emitStmt(struct EmitState *state, struct StmtAST *stmt);

void emitIf(struct EmitState *state, struct StmtAST *stmt) {
  struct Value cond = emitExpr(state, stmt->expr);
  cond = makeBool(state, cond);

  const i8 *falseLabel = "false";
  if (stmt->stmt == NULL) {
    falseLabel = "cont";
  }
  i32 idx = state->tmpCounter++;
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

void emitWhile(struct EmitState *state, struct StmtAST *stmt) {
  i32 idx = state->tmpCounter++;

  printf("  br label %%while.cond.%d\n", idx);
  printf("while.cond.%d:\n", idx);
  struct Value cond = makeBool(state, emitExpr(state, stmt->expr));
  printf("  br i1 %s, label %%while.body.%d, label %%while.cont.%d\n", cond.val,
         idx, idx);

  struct EmitState whileState = {0};
  whileState.tmpCounter = state->tmpCounter;
  whileState.parent = state;

  i8 *buf = malloc(32);
  sprintf(buf, "while.cont.%d", idx);
  whileState.curBreakLabel = buf;

  printf("while.body.%d:\n", idx);
  emitStmt(&whileState, stmt->stmt);
  printf(" br label %%while.cond.%d\n", idx);

  printf("while.cont.%d:\n", idx);

  state->tmpCounter = whileState.tmpCounter;
}

void emitFor(struct EmitState *state, struct StmtAST *stmt) {
  i32 idx = state->tmpCounter++;

  struct EmitState forState = {0};
  forState.tmpCounter = state->tmpCounter;
  forState.parent = state;

  i8 *buf = malloc(32);
  sprintf(buf, "for.cont.%d", idx);
  forState.curBreakLabel = buf;

  emitStmt(&forState, stmt->init);

  printf("  br label %%for.cond.%d\n", idx);
  printf("for.cond.%d:\n", idx);
  // cond must be an expression stmt, parseFor guarantees it.
  struct Value cond =
      makeBool(&forState, emitExpr(&forState, stmt->cond->expr));
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

  state->tmpCounter = forState.tmpCounter;
}

void emitSwitch(struct EmitState *state, struct StmtAST *stmt) {
  i32 idx = state->tmpCounter++;

  struct EmitState switchState = {0};
  switchState.tmpCounter = state->tmpCounter;
  switchState.parent = state;

  i8 *buf = malloc(32);
  sprintf(buf, "cont.%d", idx);
  switchState.curBreakLabel = buf;

  struct Value expr = emitExpr(&switchState, stmt->expr);
  printf("  br label %%switch.%d\n", idx);

  emitStmt(&switchState, stmt->stmt);

  // fallthrough to the end of the switch
  printf("  br label %%cont.%d\n", idx);

  printf("switch.%d:\n", idx);

  if (switchState.defaultLabel != NULL) {
    printf("  switch %s %s, label %%%s [\n", expr.type, expr.val,
           switchState.defaultLabel);
  } else {
    printf("  switch %s %s, label %%cont.%d [\n", expr.type, expr.val, idx);
  }
  for (struct Case *cse = switchState.cases; cse != NULL; cse = cse->next) {
    printf("    %s %s, label %%case.%d\n", cse->val.type, cse->val.val, cse->n);
  }
  printf("  ]\n");
  printf("cont.%d:\n", idx);

  state->tmpCounter = switchState.tmpCounter;
}

void emitCase(struct EmitState *state, struct StmtAST *stmt) {
  i32 index = state->tmpCounter++;

  // fallthrough.
  printf("  br label %%case.%d\n", index);
  printf("case.%d:\n", index);

  struct Value constExpr = emitExpr(state, stmt->expr);
  struct Case *cse = calloc(1, sizeof(struct Case));
  cse->n = index;
  cse->val = constExpr;
  cse->next = state->cases;
  state->cases = cse;
}

void emitStmt(struct EmitState *state, struct StmtAST *stmt) {
  switch (stmt->kind) {
  case StmtKind::EXPR:
    if (stmt->expr != NULL) {
      emitExpr(state, stmt->expr);
    }
    break;
  case StmtKind::RETURN:
    emitReturn(state, stmt);
    break;

  case StmtKind::DECL:
    emitLocalDecl(state, stmt->decl);
    break;

  case StmtKind::COMPOUND:
    // TODO: new state?
    for (struct StmtAST *cur = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      emitStmt(state, cur);
    }
    break;

  case StmtKind::IF:
    return emitIf(state, stmt);

  case StmtKind::WHILE:
    return emitWhile(state, stmt);
  case StmtKind::FOR:
    return emitFor(state, stmt);

  case StmtKind::SWITCH:
    return emitSwitch(state, stmt);

  case StmtKind::CASE:
    return emitCase(state, stmt);

  case StmtKind::BREAK:
    if (state->curBreakLabel == NULL) {
      failEmit("Break outside loop");
    }
    printf("  br label %%%s\n", state->curBreakLabel);
    break;

  case StmtKind::DEFAULT:
    if (state->defaultLabel != NULL) {
      failEmit("Multiple default");
    }
    i32 idx = state->tmpCounter++;
    state->defaultLabel = malloc(32);
    sprintf(state->defaultLabel, "default.%d", idx);
    printf("  br label %%default.%d\n", idx);
    printf("default.%d:\n", idx);
    break;
  }
}

void emitFunc(struct EmitState *state, struct DeclAST *decl) {
  struct Value val = getGlobal(decl->name);
  val.type = "ptr";
  addLocal(state, decl->name, val);

  if (decl->hasDef && decl->body == NULL) {
    return;
  }

  const i8 *defOrDecl = decl->body == NULL ? "declare" : "define";
  printf("%s %s %s(", defOrDecl, convertType(decl->type->result), val.val);
  for (struct DeclAST *arg = decl->fields; arg != NULL; arg = arg->next) {
    i64 len = arg->name.end - arg->name.data;
    printf("%s %%%.*s", convertType(arg->type), len, arg->name.data);
    if (arg->next != NULL) {
      printf(", ");
    }
  }
  printf(")");

  if (decl->body != NULL) {
    struct EmitState funcState = {0};
    funcState.parent = state;
    funcState.tmpCounter = 0;

    printf(" {\n");

    for (struct DeclAST *arg = decl->fields; arg != NULL; arg = arg->next) {
      struct Value addr = emitLocalVar(&funcState, arg);
      i64 len = arg->name.end - arg->name.data;
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

void emitStruct(struct EmitState *state, struct DeclAST *decl) {
  // emit nested structs
  for (struct DeclAST *field = decl->fields; field != NULL;
       field = field->next) {
    if (field->kind == DeclKind::STRUCT) {
      emitStruct(state, field);
    }
  }

  // TODO: padding
  printf("%s = type <{ ", convertType(decl->type));

  for (struct DeclAST *field = decl->fields; field != NULL;
       field = field->next) {
    printf("%s", convertType(field->type));
    if (field->next != NULL) {
      printf(", ");
    }
  }

  printf(" }>\n");
}

void emitGlobalVar(struct EmitState *state, struct DeclAST *decl) {
  const i8 *declSpec = decl->type->isConst ? "constant" : "global";

  struct Value val = getGlobal(decl->name);
  val.type = convertType(decl->type);
  if (decl->init != NULL) {
    struct Value init = emitExpr(state, decl->init); // TODO: emit constant
    printf("%s = %s %s %s\n", val.val, declSpec, init.type, init.val);
  } else {
    const i8 *init =
        decl->type->kind == TypeKind::STRUCT ? "zeroinitializer" : "null";
    printf("%s = %s %s %s\n", val.val, declSpec, val.type, init);
  }

  addLocal(state, decl->name, val);
}

void emitGlobalDecl(struct EmitState *state, struct DeclAST *decl) {
  switch (decl->kind) {
  case DeclKind::ENUM:
    // Enum type declarations are not emitted.
    if (decl->type->kind != TypeKind::INT) {
      return;
    }
  case DeclKind::VAR:
    emitGlobalVar(state, decl);
    break;
  case DeclKind::STRUCT:
    emitStruct(state, decl);
    break;
  case DeclKind::FUNC:
    emitFunc(state, decl);
    break;
  case DeclKind::ENUM_FIELD:
    failEmit("Unsupported");
    break;
  }
}

void emitTopLevel(struct EmitState *state, struct DeclAST *decl) {
  // TODO: emit types, globals, func decls then func defs?

  while (decl != NULL) {
    emitGlobalDecl(state, decl);
    decl = decl->next;
  }
}
