import state;
import type;

import utils;
import util;

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
    case TokenKind::GREATER:
      instr = "icmp sgt";
    case TokenKind::LE_OP:
      instr = "icmp sle";
    case TokenKind::GE_OP:
      instr = "icmp sge";
    case TokenKind::EQ_OP:
      instr = "icmp eq";
    case TokenKind::NE_OP:
      instr = "icmp ne";
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
