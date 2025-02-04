import libc;
import ast;
import print_ast;
import util;

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

func isAggregate(type: Type*) -> i32 {
  return type->kind as TypeKind::Struct* != null
      || type->kind as TypeKind::Union* != null;
}


// Convert type to LLVM type.
func convertType(type: Type*) -> const i8* {
  switch (type->kind) {
    case TypeKind::Void:
      return "void";

    case TypeKind::Int as int:
      let buf: i8* = malloc(16);
      sprintf(buf, "i%d", int.size);
      return buf;

    case TypeKind::Pointer:
      return "ptr";

    case TypeKind::Struct as s:
      let len = s.tag.end - s.tag.data;
      let buf: i8* = null;
      if (s.parent != null) {
        let parent = s.parent->kind as TypeKind::Union*;
        let parentLen = parent->tag.end - parent->tag.data;
        buf = malloc((len + parentLen + 10) as u64);
        sprintf(
            buf,
            "%%struct.%.*s.%.*s",
            parentLen,
            parent->tag.data,
            len,
            s.tag.data);
      } else {
        buf = malloc((len + 10) as u64);
        sprintf(buf, "%%struct.%.*s", len, s.tag.data);
      }
      return buf;

    case TypeKind::Union as u:
      let len = u.tag.end - u.tag.data;
      let buf: i8* = malloc((len + 10) as u64);
      sprintf(buf, "%%union.%.*s", len, u.tag.data);
      return buf;

    case TypeKind::Array as arr:
      let buf: i8* = malloc(32);
      sprintf(buf, "[%d x %s]", arr.size, convertType(arr.element));
      return buf;

    case TypeKind::Func as fn:
      let buf: i8* = malloc(128);
      let cur = buf + sprintf(buf, "%s (", convertType(fn.result));
      for (let arg = fn.args; arg != null; arg = arg->next) {
        cur += sprintf(cur, "%s", convertType(arg));
        if (arg->next != null) {
          cur += sprintf(cur, ", ");
        }
      }
      if (fn.isVarargs) {
        cur += sprintf(cur, ", ...");
      }
      sprintf(cur, ")");
      return buf;

    case TypeKind::Enum:
      return "i32";

    case TypeKind::Tag:
      failEmit("Unknown type to emit");
  }

  return null;
}


func newLocal(name: Token, val: Value) -> LocalVar* {
  let local: LocalVar* = calloc(1, sizeof(struct LocalVar));
  local->name = name;
  local->value = val;
  return local;
}

func intToVal(num: i32, type: Type*) -> Value {
  if (let ptrType = type->kind as TypeKind::Pointer*) {
    if (num != 0) {
      failEmit("expected null pointer");
    }
    return Value {
      type = "ptr",
      val = "null",
    };
  }

  let buf: i8* = malloc(16);
  sprintf(buf, "%d", num);
  return Value {
    type = convertType(type),
    val = buf,
  };
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

func emitExpr(state: EmitState*, expr: ExprAST*) -> Value;


// Turns an i1 into an i32
func upcasti1(state: EmitState*, val: Value) -> Value {
  let up = getNextTemp(state);
  up.type = val.type;
  if (strcmp(up.type, "ptr") == 0) {
    failEmit("bool to pointer?");
  }
  printf("  %s = zext i1 %s to %s\n", up.val, val.val, up.type);
  return up;
}


// Turns an i32 into an i1
func makeBool(state: EmitState*, val: Value) -> Value {
  let up = getNextTemp(state);
  up.type = val.type;
  printf("  %s = icmp ne %s %s, 0\n", up.val, val.type, val.val);
  return up;
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

func emitStructGEP(
    state: EmitState*,
    aggType: Type*,
    agg: Value,
    idx: i32
) -> Value {
  let gep = getNextTemp(state);
  gep.type = "ptr";
  printf(
      "  %s = getelementptr inbounds %s, ptr %s, i32 0, i32 %d\n",
      gep.val,
      convertType(aggType),
      agg.val,
      idx);
  return gep;
}

func emitAddr(state: EmitState*, expr: ExprAST*) -> Value {
  switch (expr->kind) {
    case ExprKind::VARIABLE:
      return lookupVar(state, expr->identifier);

    case ExprKind::INDEX:
      if (let arrayType = expr->lhs->type->kind as TypeKind::Array*) {
        let array = emitAddr(state, expr->lhs);
        let index = emitExpr(state, expr->rhs);

        let gep = getNextTemp(state);
        gep.type = "ptr";

        printf(
            "  %s = getelementptr inbounds %s, ptr %s, %s %s\n",
            gep.val,
            convertType(arrayType->element),
            array.val,
            index.type,
            index.val);
        return gep;
      } else {
        failEmitExpr(expr, "Unsupported index on non array");
      }

    case ExprKind::MEMBER:
      let agg = expr->op.kind == TokenKind::DOT
           ? emitAddr(state, expr->lhs)
           : emitExpr(state, expr->lhs);

      let aggType: Type* = null;
      if (expr->op.kind == TokenKind::DOT) {
        aggType = expr->lhs->type;
      } else {
        aggType = (expr->lhs->type->kind as TypeKind::Pointer*)->pointee;
      }

      return emitStructGEP(state, aggType, agg, expr->value);

    case ExprKind::UNARY:
      if (expr->op.kind == TokenKind::STAR) {
        return emitExpr(state, expr->rhs);
      }
    case ExprKind::PAREN:
      return emitAddr(state, expr->lhs);
    case ExprKind::CALL:
      if (!isAggregate(expr->type)) {
        break;
      }
      return emitExpr(state, expr);

    default:
      break;
  }

  failEmitExpr(expr, " Can't be use as lvalue");
  return Value {};
}

func getLLVMSize(type: Type*) -> Value {
  let res = Value {};
  res.val = malloc(128);
  sprintf(
      res.val,
      "ptrtoint (ptr getelementptr (%s, ptr null, i32 1) to i32)",
      convertType(type));
  res.type = "i32";
  return res;
}

func emitMemcpy(addr: Value, val: Value, type: Type*) {
  let size = getLLVMSize(type);
  printf(
      "  call void @llvm.memcpy.p0.p0.i32(ptr %s, ptr %s, %s %s, i1 0)\n",
      addr.val,
      val.val,
      size.type,
      size.val);
}

func emitStore(addr: Value, val: Value, type: Type*) {
  if (isAggregate(type)) {
    emitMemcpy(addr, val, type);
  } else {
    printf("  store %s %s, ptr %s\n", val.type, val.val, addr.val);
  }
}

func emitRawLoad(state: EmitState*, addr: Value, type: Type*) -> Value {
  let val = getNextTemp(state);
  val.type = convertType(type);
  printf("  %s = load %s, ptr %s\n", val.val, val.type, addr.val);
  return val;
}

func emitLoad(state: EmitState*, addr: Value, type: Type*) -> Value {
  // Funcs and arrays are implictly converted to pointers here.
  if (type->kind as TypeKind::Func* != null
      || type->kind as TypeKind::Array* != null) {
    return addr;
  }

  // Structs are stored on stack
  if (isAggregate(type)) {
    return addr;
  }

  return emitRawLoad(state, addr, type);
}

func emitAlloca(state: EmitState*, type: Type*) -> Value {
  let res = getNextTemp(state);
  res.type = "ptr";
  printf("  %s = alloca %s\n", res.val, convertType(type));
  return res;
}


func emitBinary(
    state: EmitState*,
    resType: Type*,
    opKind: TokenKind,
    lhs: Value,
    lhsType: Type*,
    rhs: Value,
    rhsType: Type*
) -> Value {
  let lhsPointer = lhsType->kind as TypeKind::Pointer*;
  let lhsIsPointer = lhsPointer != null;
  let rhsPointer = rhsType->kind as TypeKind::Pointer*;
  let rhsIsPointer = rhsPointer != null;

  // Pointer sub.
  if (lhsIsPointer && rhsIsPointer && opKind == TokenKind::MINUS) {
    // ptrtoint
    let lhsInt = getNextTemp(state);
    lhsInt.type = convertType(resType);
    printf(
        "  %s = ptrtoint %s %s to %s\n",
        lhsInt.val,
        lhs.type,
        lhs.val,
        lhsInt.type);

    // ptrtoint
    let rhsInt = getNextTemp(state);
    rhsInt.type = lhsInt.type;
    printf(
        "  %s = ptrtoint %s %s to %s\n",
        rhsInt.val,
        rhs.type,
        rhs.val,
        rhsInt.type);

    // sub
    let res = getNextTemp(state);
    res.type = lhsInt.type;
    printf("  %s = sub %s %s, %s\n", res.val, res.type, lhsInt.val, rhsInt.val);

    return res;
  }

  if (lhsIsPointer != rhsIsPointer) {
    let ptrType = lhsIsPointer ? lhsPointer : rhsPointer;
    let ptrOp = lhsIsPointer ? lhs : rhs;
    let intOp = lhsIsPointer ? rhs : lhs;

    // negate the i32 for minus op
    if (opKind == TokenKind::MINUS) {
      let neg = getNextTemp(state);
      neg.type = intOp.type;
      printf("  %s = sub %s 0, %s\n", neg.val, neg.type, intOp.val);
      intOp = neg;
    }

    let res = getNextTemp(state);
    res.type = convertType(resType);
    printf(
        "  %s = getelementptr inbounds %s, ptr %s, %s %s\n",
        res.val,
        convertType(ptrType->pointee),
        ptrOp.val,
        intOp.type,
        intOp.val);
    return res;
  }

  if (strcmp(lhs.type, rhs.type) != 0) {
    printf("%s <> %s ", lhs.type, rhs.type);

    // failEmit("Lhs and rhs don't have same type!");
    return Value {};
  }

  let instr: const i8* = null;
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

func emitAssignment(state: EmitState*, expr: ExprAST*) -> Value {
  let addr = emitAddr(state, expr->lhs);
  let val = emitExpr(state, expr->rhs);

  if (expr->op.kind == TokenKind::EQ) {
    emitStore(addr, val, expr->rhs->type);
    return val;
  }

  let lval = emitLoad(state, addr, expr->lhs->type);

  let op = TokenKind::TOK_EOF;
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

  let res = emitBinary(
      state,
      expr->type,
      op,
      lval,
      expr->lhs->type,
      val,
      expr->rhs->type);
  if (res.type == null) {
    failEmitExpr(expr, "Bin op...");
  }

  emitStore(addr, res, expr->type);
  return res;
}

func emitLogicalBinOp(state: EmitState*, expr: ExprAST*) -> Value {
  let lhs = emitExpr(state, expr->lhs);
  let idx = getCount(state);

  let firstLabel: i8* = "true";
  let secondLabel: i8* = "false";
  if (expr->op.kind == TokenKind::OR_OP) {
    firstLabel = "false";
    secondLabel = "true";
  }

  let firstCmp = getNextTemp(state);
  printf("  br label %%entry.%d\n", idx);
  printf("entry.%d:\n", idx);
  printf("  %s = icmp ne %s %s, 0\n", firstCmp.val, lhs.type, lhs.val);
  printf(
      "  br i1 %s, label %%%s.%d, label %%%s.%d\n",
      firstCmp.val,
      firstLabel,
      idx,
      secondLabel,
      idx);

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
  printf(
      "  %s = phi i1 [ %s, %%entry.%d ], [ %s, %%true.cont.%d ]\n",
      res.val,
      secondLabel,
      idx,
      secondCmp.val,
      idx);

  return upcasti1(state, res);
}


func emitBinOp(state: EmitState*, expr: ExprAST*) -> Value {
  if (isAssign(expr->op)) {
    return emitAssignment(state, expr);
  }

  if (expr->op.kind == TokenKind::AND_OP || expr->op.kind == TokenKind::OR_OP) {
    return emitLogicalBinOp(state, expr);
  }

  let lhs = emitExpr(state, expr->lhs);
  let rhs = emitExpr(state, expr->rhs);

  if (expr->op.kind == TokenKind::COMMA) {
    return rhs;
  }

  let res = emitBinary(
      state,
      expr->type,
      expr->op.kind,
      lhs,
      expr->lhs->type,
      rhs,
      expr->rhs->type);
  if (res.type == null) {
    failEmitExpr(expr, "Bin op difference");
  }
  return res;
}

func emitUnary(state: EmitState*, expr: ExprAST*) -> Value {
  // Use emitBinary to handle the inc/dec, as it'll handle pointer
  // types correctly.
  if (expr->op.kind == TokenKind::INC_OP
      || expr->op.kind == TokenKind::DEC_OP) {
    let opExpr = expr->lhs == null ? expr->rhs : expr->lhs;
    let operand = emitAddr(state, opExpr);

    let val = emitLoad(state, operand, opExpr->type);

    let type = opExpr->type->kind as TypeKind::Pointer* != null
         ? getInt32()
         : opExpr->type;
    let one = intToVal(expr->op.kind == TokenKind::INC_OP ? 1 : -1, type);
    let res = emitBinary(
        state,
        opExpr->type,
        TokenKind::PLUS,
        val,
        opExpr->type,
        one,
        type);
    if (res.type == null) {
      failEmitExpr(expr, "Bin op diff");
    }
    emitStore(operand, res, opExpr->type);

    if (expr->lhs != null) {
      return val;
    }
    return res;
  }

  let operand = emitExpr(state, expr->rhs);
  let res = getNextTemp(state);

  let instr: const i8* = null;
  let constop: const i8* = null;
  let upcast = 0;
  switch (expr->op.kind) {
    case TokenKind::PLUS:
      return operand;

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
  printf(
      "  %s = %s %s %s, %s\n",
      res.val,
      instr,
      operand.type,
      constop,
      operand.val);

  if (upcast) {
    return upcasti1(state, res);
  }

  return res;
}

func getStrConst(type: Type*, tok: Token) -> Value {
  let len = tok.end - tok.data;
  let val: i8* = malloc((len + 16) as u64);

  let cur = val;
  cur += sprintf(val, "c\"");  // %.*s\\00\"", len, tok.data);

  for (let i: i64 = 0; i < len; i++) {
    let val = *(tok.data + i);
    if (val == 92) {
      let c = getEscaped(*(tok.data + ++i));
      cur += sprintf(cur, "\\%02x", c);
    } else {
      *cur++ = val;
    }
  }
  cur += sprintf(cur, "\\00\"");

  return Value {
    type = convertType(type),
    val = val,
  };
}

func emitStrRef(state: EmitState*, expr: ExprAST*) -> Value {
  let strGlobal = getTempGlobal(state, "str.");
  strGlobal.type = "ptr";

  let strType = newType(TypeKind::Array {
    element = (expr->type->kind as TypeKind::Pointer*)->pointee,
    size = (expr->identifier.end - expr->identifier.data + 1) as i32,
  });
  let strConst = getStrConst(strType, expr->identifier);

  printf("%s = constant %s %s\n", strGlobal.val, strConst.type, strConst.val);

  return strGlobal;
}

func emitArray(state: EmitState*, expr: ExprAST*) -> Value {
  let res = Value {
    type = convertType(expr->type),
  };

  let arrayType = expr->type->kind as TypeKind::Array*;
  let buf: i8* = malloc((64 * arrayType->size as i64) as u64);
  res.val = buf;

  buf += sprintf(buf, "[ ");

  for (let field = expr; field != null; field = field->rhs) {
    let elem = emitExpr(state, field->lhs);
    buf += sprintf(buf, "%s %s", elem.type, elem.val);
    if (field->rhs != null) {
      buf += sprintf(buf, ", ");
    }
  }
  buf += sprintf(buf, " ]");

  return res;
}

func emitCall(state: EmitState*, expr: ExprAST*) -> Value {
  let args: LocalVar* = null;
  let argsTail: LocalVar* = null;

  for (let arg = expr->rhs; arg != null; arg = arg->rhs) {
    let eof = Token {};
    let argVal = emitExpr(state, arg->lhs);

    if (isAggregate(arg->lhs->type)) {
      argVal = emitRawLoad(state, argVal, arg->lhs->type);
    }

    let nextArg = newLocal(eof, argVal);
    if (args == null) {
      args = nextArg;
    } else {
      argsTail->next = nextArg;
    }
    argsTail = nextArg;
  }
  let fn = emitExpr(state, expr->lhs);

  let res = Value {};

  if (expr->type->kind as TypeKind::Void* == null) {
    res = getNextTemp(state);
    printf("  %s = ", res.val);
  } else {
    res.val = "undef";
    printf("  ");
  }

  res.type = convertType(expr->type);
  printf("call %s %s(", convertType(expr->lhs->type), fn.val);

  for (; args != null; args = args->next) {
    printf("%s %s", args->value.type, args->value.val);
    if (args->next != null) {
      printf(", ");
    }
  }
  printf(")\n");

  if (!isAggregate(expr->type)) {
    return res;
  }

  let alloc = emitAlloca(state, expr->type);
  printf("  store %s %s, ptr %s\n", res.type, res.val, alloc.val);
  return alloc;
}

func emitCast(state: EmitState*, expr: ExprAST*) -> Value {
  let v = emitExpr(state, expr->lhs);

  let from = expr->lhs->type;
  let to = expr->type;
  switch (expr->castKind) {
    case CastKind::Noop:
      return v;

    case CastKind::StructUnion:
      let res = emitAlloca(state, to);

      let kindAddr = emitStructGEP(state, to, res, 0);
      let kindType = getInt32();
      emitStore(kindAddr, intToVal(expr->value, kindType), kindType);

      let valAddr = emitStructGEP(state, to, res, 1);
      emitMemcpy(valAddr, v, from);

      return res;

    case CastKind::UnionStructPtr:
      let unionType = (from->kind as TypeKind::Pointer*)->pointee;

      let kindGep = emitStructGEP(state, unionType, v, 0);
      let kind = emitLoad(state, kindGep, getInt32());
      let valGep = emitStructGEP(state, unionType, v, 1);

      // cmpRes = kind == expr->value;
      let cmpRes = getNextTemp(state);
      printf(
          "  %s = icmp eq %s %s, %d\n",
          cmpRes.val,
          kind.type,
          kind.val,
          expr->value);

      // res = select cmpRes, valGep, null
      let res = getNextTemp(state);
      res.type = convertType(to);
      printf(
          "  %s = select i1 %s, %s %s, ptr null\n",
          res.val,
          cmpRes.val,
          valGep.type,
          valGep.val);
      return res;

    case CastKind::Trunc:
      let res = getNextTemp(state);
      res.type = convertType(to);
      printf("  %s = trunc %s %s to %s\n", res.val, v.type, v.val, res.type);
      return res;
    case CastKind::Sext:
      let res = getNextTemp(state);
      res.type = convertType(to);
      printf("  %s = sext %s %s to %s\n", res.val, v.type, v.val, res.type);
      return res;
    case CastKind::Zext:
      let res = getNextTemp(state);
      res.type = convertType(to);
      printf("  %s = zext %s %s to %s\n", res.val, v.type, v.val, res.type);
      return res;
  }
}

func emitCond(state: EmitState*, expr: ExprAST*) -> Value {
  let cond = emitExpr(state, expr->cond);
  cond = makeBool(state, cond);

  let falseLabel: i8* = "false";
  let idx = getCount(state);
  printf(
      "  br i1 %s, label %%cond.true.%d, label %%cond.%s.%d\n",
      cond.val,
      idx,
      falseLabel,
      idx);

  printf("cond.true.%d:\n", idx);
  let trueVal = emitExpr(state, expr->lhs);
  printf("  br label %%cond.cont.%d\n", idx);

  printf("cond.false.%d:\n", idx);
  let falseVal = emitExpr(state, expr->rhs);
  printf("  br label %%cond.cont.%d\n", idx);

  printf("cond.cont.%d:\n", idx);

  let res = getNextTemp(state);
  res.type = trueVal.type;
  printf(
      "  %s = phi %s [ %s, %%cond.true.%d ], [ %s, %%cond.false.%d ]\n",
      res.val,
      res.type,
      trueVal.val,
      idx,
      falseVal.val,
      idx);
  return res;
}

func emitStructExpr(state: EmitState*, expr: ExprAST*) -> Value {
  let res = emitAlloca(state, expr->type);
  printf("  store %s zeroinitializer, ptr %s\n", convertType(expr->type), res.val);
  for (let field = expr->rhs; field != null; field = field->rhs) {
    let fieldVal = emitExpr(state, field->lhs);
    let fieldGep = emitStructGEP(state, expr->type, res, field->value);

    emitStore(fieldGep, fieldVal, field->lhs->type);
  }

  return res;
}


func addLocal(state: EmitState*, name: Token, val: Value) {
  let local = newLocal(name, val);
  local->next = state->vars;
  state->vars = local;
}

func emitLocalVar(state: EmitState*, decl: DeclAST*) -> Value {
  let val = emitAlloca(state, decl->type);

  let local = newLocal(decl->name, val);
  local->next = state->vars;
  state->vars = local;

  return val;
}


// \returns The register name or value of the expr
func emitExpr(state: EmitState*, expr: ExprAST*) -> Value {
  switch (expr->kind) {
    case ExprKind::INT:
      return intToVal(expr->value, expr->type);
    case ExprKind::SCOPE:
      return intToVal(expr->value, expr->type);
    case ExprKind::BINARY:
      return emitBinOp(state, expr);

    case ExprKind::UNARY:
      if (expr->op.kind == TokenKind::AND) {
        let res = emitAddr(state, expr->rhs);
        res.type = "ptr";
        return res;
      }

      if (expr->op.kind == TokenKind::STAR) {
        let res = emitExpr(state, expr->rhs);
        return emitLoad(state, res, expr->type);
      }

      return emitUnary(state, expr);

    case ExprKind::VARIABLE:
      let addr = emitAddr(state, expr);
      return emitLoad(state, addr, expr->type);
    case ExprKind::INDEX, ExprKind::MEMBER:
      let addr = emitAddr(state, expr);
      return emitLoad(state, addr, expr->type);

    case ExprKind::STR:
      if (expr->type->kind as TypeKind::Array* != null) {
        return getStrConst(expr->type, expr->identifier);
      }
      return emitStrRef(state, expr);

    case ExprKind::ARRAY:
      return emitArray(state, expr);

    case ExprKind::CALL:
      return emitCall(state, expr);

    case ExprKind::CAST:
      return emitCast(state, expr);

    case ExprKind::CONDITIONAL:
      return emitCond(state, expr);

    case ExprKind::STRUCT:
      return emitStructExpr(state, expr);
    case ExprKind::PAREN:
      return emitExpr(state, expr->lhs);

    case ExprKind::LET:
      let addr = emitLocalVar(state, expr->decl);
      let init = emitExpr(state, expr->decl->init);
      emitStore(addr, init, expr->decl->init->type);
      return init;

    case ExprKind::ARG_LIST, ExprKind::SIZEOF:
      failEmitExpr(expr, "Shouldn't happen");
  }

  return Value {
    type = "i32",
    val = "undef",
  };
}

func emitReturn(state: EmitState*, stmt: StmtAST*) {
  if (stmt->expr == null) {
    printf("  ret void\n");
    return;
  }
  let expr = stmt->expr;

  let v = emitExpr(state, expr);
  if (expr->type->kind as TypeKind::Void* != null) {
    printf("  ret void\n");
    return;
  }

  if (isAggregate(expr->type)) {
    v = emitRawLoad(state, v, expr->type);
  }

  printf("  ret %s %s\n", v.type, v.val);
}


func emitStmt(state: EmitState*, stmt: StmtAST*);

func emitIf(state: EmitState*, stmt: StmtAST*) {
  let cond = emitExpr(state, stmt->expr);
  cond = makeBool(state, cond);

  let falseLabel: i8* = "false";
  if (stmt->stmt == null) {
    falseLabel = "cont";
  }
  let idx = getCount(state);
  printf(
      "  br i1 %s, label %%if.true.%d, label %%if.%s.%d\n",
      cond.val,
      idx,
      falseLabel,
      idx);

  printf("if.true.%d:\n", idx);
  emitStmt(state, stmt->init);
  printf("  br label %%if.cont.%d\n", idx);

  if (stmt->stmt != null) {
    printf("if.false.%d:\n", idx);
    emitStmt(state, stmt->stmt);
    printf("  br label %%if.cont.%d\n", idx);
  }

  printf("if.cont.%d:\n", idx);
}

func emitWhile(state: EmitState*, stmt: StmtAST*) {
  let idx = getCount(state);

  printf("  br label %%while.cond.%d\n", idx);
  printf("while.cond.%d:\n", idx);
  let cond = makeBool(state, emitExpr(state, stmt->expr));
  printf(
      "  br i1 %s, label %%while.body.%d, label %%while.cont.%d\n",
      cond.val,
      idx,
      idx);

  let whileState = newEmitState(state);

  let buf: i8* = malloc(32);
  sprintf(buf, "while.cont.%d", idx);
  whileState.curBreakLabel = buf;

  printf("while.body.%d:\n", idx);
  emitStmt(&whileState, stmt->stmt);
  printf(" br label %%while.cond.%d\n", idx);

  printf("while.cont.%d:\n", idx);
}

func emitFor(state: EmitState*, stmt: StmtAST*) {
  let idx = getCount(state);

  let forState = newEmitState(state);

  let buf: i8* = malloc(32);
  sprintf(buf, "for.cont.%d", idx);
  forState.curBreakLabel = buf;

  emitStmt(&forState, stmt->init);

  printf("  br label %%for.cond.%d\n", idx);
  printf("for.cond.%d:\n", idx);

  // cond must be an expression stmt, parseFor guarantees it.
  let cond = makeBool(&forState, emitExpr(&forState, stmt->cond->expr));
  printf(
      "  br i1 %s, label %%for.body.%d, label %%for.cont.%d\n",
      cond.val,
      idx,
      idx);

  printf("for.body.%d:\n", idx);
  emitStmt(&forState, stmt->stmt);
  printf("  br label %%for.incr.%d\n", idx);

  // TODO: continue would jump here
  printf("for.incr.%d:\n", idx);
  emitExpr(&forState, stmt->expr);
  printf("  br label %%for.cond.%d\n", idx);

  printf("for.cont.%d:\n", idx);
}

func getCases(
    state: EmitState*,
    expr: ExprAST*,
    cases: Case*,
    index: i32
) -> Case* {
  switch (expr->kind) {
    case ExprKind::SCOPE, ExprKind::INT:
      let cse: Case* = calloc(1, sizeof(struct Case));
      cse->n = index;
      if (expr->type->kind as TypeKind::Union* != null) {
        cse->val = intToVal(expr->value, getInt32());
      } else {
        cse->val = emitExpr(state, expr);
      }
      cse->next = cases;
      return cse;

    case ExprKind::BINARY:
      let lhsCases = getCases(state, expr->lhs, cases, index);
      return getCases(state, expr->rhs, lhsCases, index);

    case ExprKind::MEMBER:
      if (state->switchUnionAddr.val == null) {
        failEmitExpr(expr, "case as on non union type?");
      }

      let val = emitStructGEP(state, expr->type, state->switchUnionAddr, 1);
      let local = newLocal(expr->identifier, val);
      local->next = state->vars;
      state->vars = local;

      let cse: Case* = calloc(1, sizeof(struct Case));
      cse->n = index;
      cse->val = intToVal(expr->value, getInt32());
      cse->next = cases;
      return cse;

    default:
      failEmitExpr(expr, "Unsupported case expr");
  }
}

func emitSwitch(state: EmitState*, stmt: StmtAST*) {
  let switchIdx = getCount(state);

  let switchState = newEmitState(state);

  let buf: i8* = malloc(32);
  sprintf(buf, "cont.%d", switchIdx);
  switchState.curBreakLabel = buf;

  let switchExpr = stmt->expr;

  // let isPointer = switchExpr->type->kind == TypeKind::POINTER;
  // || (isPointer && switchExpr->type->arg->kind == TypeKind::UNION);
  let isUnion = switchExpr->type->kind as TypeKind::Union* != null;

  let expr = Value {};
  if (isUnion) {
    let unionAddr = emitAddr(&switchState, switchExpr);

    // isPointer ? emitExpr(&switchState, switchExpr) :;
    // isPointer ? switchExpr->type->arg : switchExpr->type;
    let unionType = switchExpr->type;

    // Load the first element, which is the i32 kind.
    let gep = emitStructGEP(&switchState, unionType, unionAddr, 0);

    // TODO: don't hardcode type.
    expr = emitLoad(&switchState, gep, getInt32());
    switchState.switchUnionAddr = unionAddr;
  } else {
    expr = emitExpr(&switchState, switchExpr);
  }
  printf("  br label %%switch.%d\n", switchIdx);

  let cases: Case* = null;
  let defaultLabel: i8* = null;

  for (let caseStmt = stmt->stmt; caseStmt != null;
       caseStmt = caseStmt->nextStmt) {
    // Jump to end of switch at the end of the previous case.
    printf("  br label %%cont.%d\n", switchIdx);
    let caseState = newEmitState(&switchState);

    if (caseStmt->kind == StmtKind::CASE) {
      let caseIdx = getCount(&caseState);

      printf("case.%d:\n", caseIdx);

      cases = getCases(&caseState, caseStmt->expr, cases, caseIdx);
    } else if (caseStmt->kind == StmtKind::DEFAULT) {
      if (defaultLabel != null) {
        failEmit("Multiple default");
      }

      let defaultIdx = getCount(&caseState);
      defaultLabel = malloc(32);
      sprintf(defaultLabel, "default.%d", defaultIdx);

      printf("default.%d:\n", defaultIdx);
    } else {
      failEmit("Unsupported switch stmt");
    }

    for (let cur = caseStmt->stmt; cur != null; cur = cur->nextStmt) {
      emitStmt(&caseState, cur);
    }
  }

  // fallthrough to the end of the switch
  printf("  br label %%cont.%d\n", switchIdx);

  printf("switch.%d:\n", switchIdx);

  if (defaultLabel != null) {
    printf(
        "  switch %s %s, label %%%s [\n",
        expr.type,
        expr.val,
        defaultLabel);
  } else {
    printf(
        "  switch %s %s, label %%cont.%d [\n",
        expr.type,
        expr.val,
        switchIdx);
  }
  for (let cse = cases; cse != null; cse = cse->next) {
    printf("    %s %s, label %%case.%d\n", cse->val.type, cse->val.val, cse->n);
  }
  printf("  ]\n");
  printf("cont.%d:\n", switchIdx);
}

func emitStmt(state: EmitState*, stmt: StmtAST*) {
  switch (stmt->kind) {
    case StmtKind::EXPR:
      if (stmt->expr != null) {
        emitExpr(state, stmt->expr);
      }
    case StmtKind::RETURN:
      emitReturn(state, stmt);
    case StmtKind::COMPOUND:
      let newState = newEmitState(state);
      for (let cur = stmt->stmt; cur != null; cur = cur->nextStmt) {
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
      if (state->curBreakLabel == null) {
        failEmit("Break outside loop");
      }
      printf("  br label %%%s\n", state->curBreakLabel);
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

func emitTopLevel(decl: DeclAST*) {
  let rootCounter = 0;
  let state = EmitState {
    tmpCounter = &rootCounter,
  };

  for (let cur = decl; cur != null; cur = cur->next) {
    addGlobalDecl(&state, cur);
  }

  for (let cur = decl; cur != null; cur = cur->next) {
    emitGlobalDecl(&state, cur);
  }
}
