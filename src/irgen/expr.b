import ir;
import ir.type;
import ast;
import ast.print;
import util;

import irgen.state;
import irgen.utils;

func genConstant(state: IRGenState*, expr: ExprAST*) -> Value {
  switch (expr->kind) {
    case ExprKind::INT, ExprKind::SCOPE:
      if (expr->type->kind as TypeKind::Pointer* != null) {
        if (expr->value != 0) {
          failIRGen("Only null constants supported");
        }
        return Value::Zero {
          type = expr->type,
        };
      }

      return Value::IntConstant {
        value = expr->value,
        type = expr->type,
      };

    case ExprKind::STR:
      return Value::StrConstant {
        value = expr->identifier,
        type = expr->type,
      };

    case ExprKind::ARRAY:
      let arrayType = expr->type->kind as TypeKind::Array*;
      let size = arrayType->size as u32;
      let values = calloc(size as u64, sizeof(union Value)) as Value*;

      let i = 0;
      for (let field = expr; field != null; field = field->rhs, i++) {
        *(values + i) = genConstant(state, field->lhs);
      }
      return Value::ArrayConstant {
        type = expr->type,
        values = values,
        size = arrayType->size,
      };

    // Address of global
    // TODO: allow offsets?
    case ExprKind::UNARY:
      if (expr->op.kind != TokenKind::AND) {
        break;
      }
      let globalVar = expr->rhs;
      if (globalVar->kind != ExprKind::VARIABLE) {
        break;
      }
      let var = findName(state, globalVar->identifier);
      if (var == null) {
        break;
      }

      if (let globalPtr = var as Value::GlobalPtr*) {
        return *var;
      }

    default:
      break;
  }

  printExpr(expr);
  printf("\n");
  failIRGen("TODO: constant exprs");
}

func genAddr(state: IRGenState*, expr: ExprAST*) -> Value {
  switch (expr->kind) {
    case ExprKind::VARIABLE:
      let var = findName(state, expr->identifier);
      if (var == null) {
        failIRGen("Failed to find variable");
      }
      return *var;

    case ExprKind::UNARY:
      if (expr->op.kind == TokenKind::STAR) {
        return genExpr(state, expr->rhs);
      }

    case ExprKind::INDEX:
      let array = genAddr(state, expr->lhs);
      let index = genExpr(state, expr->rhs);
      return addInstr(state, getPtrType(), InstrKind::ArrayGEP {
        type = expr->type,
        ptr = array,
        idx = index,
      });

    case ExprKind::MEMBER:
      let agg = expr->op.kind == TokenKind::DOT
           ? genAddr(state, expr->lhs)
           : genExpr(state, expr->lhs);

      let aggType: Type* = null;
      if (expr->op.kind == TokenKind::DOT) {
        aggType = expr->lhs->type;
      } else {
        aggType = (expr->lhs->type->kind as TypeKind::Pointer*)->pointee;
      }
      return addInstr(state, getPtrType(), InstrKind::StructGEP {
        type = aggType,
        ptr = agg,
        field = expr->value,
      });

    case ExprKind::PAREN:
      return genAddr(state, expr->lhs);

    case ExprKind::CALL:
      if (!isAggregate(expr->type)) {
        break;
      }
      return genExpr(state, expr);

    default:
      break;
  }

  let loc = expr->location;
  dprintf(STDERR, "%s:%d:%d: ", loc.fileName, loc.line, loc.column);
  failIRGen("Expr can't be used as lvalue");
  printExpr(expr);
  printf("\n");
  return Value::InstrPtr {};
}


func genExpr(state: IRGenState*, expr: ExprAST*) -> Value {
  switch (expr->kind) {
    case ExprKind::PAREN:
      return genExpr(state, expr->lhs);

    case ExprKind::INT, ExprKind::SCOPE, ExprKind::STR, ExprKind::ARRAY:
      return genConstant(state, expr);

    case ExprKind::VARIABLE, ExprKind::INDEX, ExprKind::MEMBER:
      let addr = genAddr(state, expr);
      return genLoad(state, addr, expr->type);

    case ExprKind::UNARY:
      return genUnary(state, expr);

    case ExprKind::BINARY:
      if (isAssign(expr->op)) {
        return genAssign(state, expr);
      }

      if (expr->op.kind == TokenKind::AND_OP || expr->op.kind == TokenKind::OR_OP) {
        return genLogicalBinOp(state, expr);
      }

      let lhs = genExpr(state, expr->lhs);
      let rhs = genExpr(state, expr->rhs);

      if (expr->op.kind == TokenKind::COMMA) {
        return rhs;
      }

      return genBinary(
          state,
          expr->type,
          expr->op.kind,
          lhs,
          expr->lhs->type,
          rhs,
          expr->rhs->type);

    case ExprKind::CONDITIONAL:
      return genConditional(state, expr);

    case ExprKind::CALL:
      return genCall(state, expr);

    case ExprKind::STRUCT:
      return genStructExpr(state, expr);

    case ExprKind::CAST:
      return genCast(state, expr);

    case ExprKind::LET:
      let init = genExpr(state, expr->decl->init);

      // Const expressions are handled during sema.
      if (expr->decl->kind == DeclKind::CONST) {
        return init;
      }

      // TODO: if init is an alloca, don't make a new one.
      let alloc = addAlloca(state, expr->type);
      addLocal(state, expr->decl->name, alloc);
      genStore(state, alloc, init, expr->type);

      return init;

    case ExprKind::ARG_LIST, ExprKind::SIZEOF:
      break;
  }

  failIRGen("Invalid expr");
}

func genUnary(state: IRGenState*, expr: ExprAST*) -> Value {
  switch (expr->op.kind) {
    case TokenKind::STAR:
      let addr = genExpr(state, expr->rhs);
      return genLoad(state, addr, expr->type);

    case TokenKind::AND:
      return genAddr(state, expr->rhs);

    case TokenKind::INC_OP, TokenKind::DEC_OP:
      let opExpr = expr->lhs == null ? expr->rhs : expr->lhs;
      let operand = genAddr(state, opExpr);
      let val = genLoad(state, operand, opExpr->type);
      let type = opExpr->type->kind as TypeKind::Pointer* != null
           ? getInt32()
           : opExpr->type;
      let one = Value::IntConstant {
        value = expr->op.kind == TokenKind::INC_OP ? 1 : -1,
        type = type,
      };
      let res = genBinary(
          state,
          opExpr->type,
          TokenKind::PLUS,
          val,
          opExpr->type,
          one,
          type);
      addInstr(state, null, InstrKind::Store {
        ptr = operand,
        val = res,
      });
      if (expr->lhs != null) {
        return val;
      }
      return res;

    case TokenKind::PLUS, TokenKind::MINUS:
      let op = genExpr(state, expr->rhs);
      return addInstr(state, expr->type, InstrKind::Binary {
        op = BinaryOp::Add,
        lhs = op,
        rhs = Value::IntConstant {
          value = expr->op.kind == TokenKind::PLUS ? 1 : -1,
          type = getInt32(),
        },
      });

    case TokenKind::TILDE:
      let op = genExpr(state, expr->rhs);
      return addInstr(state, expr->type, InstrKind::Binary {
        op = BinaryOp::Xor,
        lhs = op,
        rhs = Value::IntConstant {
          value = -1,
          type = getInt32(),
        },
      });

    case TokenKind::BANG:
      let op = genExpr(state, expr->rhs);
      return addInstr(state, expr->type, InstrKind::Cmp {
        op = CmpOp::Eq,
        lhs = op,
        rhs = Value::IntConstant {
          value = 0,
          type = getInt32(),
        },
      });

    default:
      failIRGen("Invalid unary");
  }
}

func genCast(state: IRGenState*, expr: ExprAST*) -> Value {
  let v = genExpr(state, expr->lhs);

  let from = expr->lhs->type;
  let to = expr->type;
  switch (expr->castKind) {
    case CastKind::Noop:
      return v;

    case CastKind::StructUnion:
      let res = addAlloca(state, to);

      let kindAddr = addInstr(state, getPtrType(), InstrKind::StructGEP {
        type = to,
        ptr = res,
        field = 0,
      });

      let kindType = getInt32();
      let kindVal = Value::IntConstant {
        value = expr->value,
        type = kindType,
      };
      genStore(state, kindAddr, kindVal, kindType);

      let valAddr = addInstr(state, getPtrType(), InstrKind::StructGEP {
        type = to,
        ptr = res,
        field = 1,
      });
      genMemcpy(state, valAddr, v, from);

      return res;

    case CastKind::UnionStructPtr:
      let unionType = (from->kind as TypeKind::Pointer*)->pointee;

      let kindGEP = addInstr(state, getPtrType(), InstrKind::StructGEP {
        type = unionType,
        ptr = v,
        field = 0,
      });
      let kindType = getInt32();
      let kind = genLoad(state, kindGEP, kindType);
      let valGEP = addInstr(state, getPtrType(), InstrKind::StructGEP {
        type = unionType,
        ptr = v,
        field = 1,
      });

      let cmpRes = addInstr(state, getBool(), InstrKind::Cmp {
        op = CmpOp::Eq,
        lhs = kind,
        rhs = Value::IntConstant {
          value = expr->value,
          type = kindType,
        },
      });
      let res = addInstr(state, to, InstrKind::Select {
        cond = cmpRes,
        trueVal = valGEP,
        falseVal = Value::Zero {
          type = to,
        },
      });
      return res;

    case CastKind::Trunc, CastKind::Sext, CastKind::Zext, CastKind::PtrToInt:
      return addInstr(state, expr->type, InstrKind::Cast {
        kind = expr->castKind,
        val = v,
      });
  }
}

func genBinary(
    state: IRGenState*,
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
    let lhsInt = addInstr(state, resType, InstrKind::Cast {
      kind = CastKind::PtrToInt,
      val = lhs,
    });

    let rhsInt = addInstr(state, resType, InstrKind::Cast {
      kind = CastKind::PtrToInt,
      val = rhs,
    });

    return addInstr(state, resType, InstrKind::Binary {
      op = BinaryOp::Sub,
      lhs = lhsInt,
      rhs = rhsInt,
    });
  }

  if (lhsIsPointer != rhsIsPointer) {
    let ptrType = lhsIsPointer ? lhsType : rhsType;
    let ptrTypeKind = lhsIsPointer ? lhsPointer : rhsPointer;
    let intType = lhsIsPointer ? rhsType : lhsType;
    let ptrOp = lhsIsPointer ? lhs : rhs;
    let intOp = lhsIsPointer ? rhs : lhs;

    // negate the i32 for minus op
    if (opKind == TokenKind::MINUS) {
      intOp = addInstr(state, intType, InstrKind::Binary {
        op = BinaryOp::Sub,
        lhs = Value::IntConstant {
          type = intType,
          value = 0,
        },
        rhs = intOp,
      });
    }

    return addInstr(state, ptrType, InstrKind::ArrayGEP {
      type = ptrTypeKind->pointee,
      ptr = ptrOp,
      idx = intOp,
    });
  }

  let isBinOp = true;
  let binOp = BinaryOp::Add;
  let cmpOp = CmpOp::Eq;

  switch (opKind) {
    default:
      failIRGen("Invalid binary op");
    case TokenKind::PLUS:
      binOp = BinaryOp::Add;
    case TokenKind::MINUS:
      binOp = BinaryOp::Sub;
    case TokenKind::STAR:
      binOp = BinaryOp::Mul;
    case TokenKind::SLASH:
      binOp = BinaryOp::SDiv;
    case TokenKind::PERCENT:
      binOp = BinaryOp::SRem;
    case TokenKind::LEFT_OP:
      binOp = BinaryOp::Shl;
    case TokenKind::RIGHT_OP:
      binOp = BinaryOp::AShr;
    case TokenKind::AND:
      binOp = BinaryOp::And;
    case TokenKind::HAT:
      binOp = BinaryOp::Xor;
    case TokenKind::PIPE:
      binOp = BinaryOp::Or;

    case TokenKind::LESS:
      isBinOp = false;
      cmpOp = CmpOp::Slt;
    case TokenKind::GREATER:
      isBinOp = false;
      cmpOp = CmpOp::Sgt;
    case TokenKind::LE_OP:
      isBinOp = false;
      cmpOp = CmpOp::Sle;
    case TokenKind::GE_OP:
      isBinOp = false;
      cmpOp = CmpOp::Sge;
    case TokenKind::EQ_OP:
      isBinOp = false;
      cmpOp = CmpOp::Eq;
    case TokenKind::NE_OP:
      isBinOp = false;
      cmpOp = CmpOp::Ne;
  }

  if (isBinOp) {
    return addInstr(state, resType, InstrKind::Binary {
      op = binOp,
      lhs = lhs,
      rhs = rhs,
    });
  }

  return addInstr(state, resType, InstrKind::Cmp {
    op = cmpOp,
    lhs = lhs,
    rhs = rhs,
  });
}

func genLogicalBinOp(state: IRGenState*, expr: ExprAST*) -> Value {
  let lhs = genExpr(state, expr->lhs);
  let entryBB = state->curBB;

  let trueBB = addBasicBlock(state, "true");
  let falseBB = addBasicBlock(state, "false");

  let firstBB = trueBB;
  let secondBB = falseBB;

  let falseResult = 0;
  if (expr->op.kind == TokenKind::OR_OP) {
    falseResult = 1;
    firstBB = falseBB;
    secondBB = trueBB;
  }

  addInstr(state, null, InstrKind::CondBranch {
    cond = lhs,
    trueBB = firstBB,
    falseBB = secondBB,
  });

  state->curBB = trueBB;
  let rhs = genExpr(state, expr->rhs);
  let exitBB = state->curBB;
  addInstr(state, null, InstrKind::Branch {
    bb = falseBB,
  });

  state->curBB = falseBB;
  return addInstr(state, expr->type, InstrKind::Phi {
    trueBB = entryBB,
    trueVal = Value::IntConstant {
      type = expr->type,
      value = falseResult,
    },
    falseBB = exitBB,
    falseVal = rhs,
  });
}

func genAssign(state: IRGenState*, expr: ExprAST*) -> Value {
  let adr = genAddr(state, expr->lhs);
  let val = genExpr(state, expr->rhs);

  if (expr->op.kind == TokenKind::EQ) {
    genStore(state, adr, val, expr->type);
    return val;
  }

  let lval = genLoad(state, adr, expr->lhs->type);

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
      failIRGen("Invalid assign op");
  }

  let res = genBinary(
      state,
      expr->type,
      op,
      lval,
      expr->lhs->type,
      val,
      expr->rhs->type);

  genStore(state, adr, res, expr->type);
  return res;
}

func genConditional(state: IRGenState*, expr: ExprAST*) -> Value {
  let cond = genExpr(state, expr->cond);

  let trueBB = addBasicBlock(state, "true");
  let falseBB = addBasicBlock(state, "false");
  let contBB = addBasicBlock(state, "cont");

  addInstr(state, null, InstrKind::CondBranch {
    cond = cond,
    trueBB = trueBB,
    falseBB = falseBB,
  });

  state->curBB = trueBB;
  let trueVal = genExpr(state, expr->lhs);
  addInstr(state, null, InstrKind::Branch {
    bb = contBB,
  });
  let trueExitBB = state->curBB;

  state->curBB = falseBB;
  let falseVal = genExpr(state, expr->rhs);
  addInstr(state, null, InstrKind::Branch {
    bb = contBB,
  });
  let falseExitBB = state->curBB;

  state->curBB = contBB;
  let type = expr->type;
  if (isAggregate(type)) {
    type = getPtrType();
  }
  return addInstr(state, type, InstrKind::Phi {
    trueBB = trueExitBB,
    trueVal = trueVal,
    falseBB = falseExitBB,
    falseVal = falseVal,
  });
}

func genStructExpr(state: IRGenState*, expr: ExprAST*) -> Value {
  let res = addAlloca(state, expr->type);
  addInstr(state, null, InstrKind::Store {
    ptr = res,
    val = Value::Zero {
      type = expr->type,
    },
  });

  for (let field = expr->rhs; field != null; field = field->rhs) {
    let fieldVal = genExpr(state, field->lhs);
    let fieldGep = addInstr(state, getPtrType(), InstrKind::StructGEP {
      type = expr->type,
      ptr = res,
      field = field->value,
    });
    genStore(state, fieldGep, fieldVal, field->lhs->type);
  }

  return res;
}

func genCall(state: IRGenState*, expr: ExprAST*) -> Value {
  let numArgs = 0 as u64;
  for (let arg = expr->rhs; arg != null; arg = arg->rhs) {
    numArgs++;
  }

  let args = calloc(numArgs, sizeof(union Value)) as Value*;
  let i = 0;
  for (let arg = expr->rhs; arg != null; arg = arg->rhs, i++) {
    let argVal = genExpr(state, arg->lhs);
    if (isAggregate(arg->lhs->type)) {
      argVal = addInstr(state, arg->lhs->type, InstrKind::Load {
        ptr = argVal,
      });
    }
    *(args + i) = argVal;
  }

  let fn = genExpr(state, expr->lhs);
  let res = addInstr(state, expr->type, InstrKind::Call {
    fn = fn,
    args = args,
    numArgs = numArgs as i32,
  });

  if (isAggregate(expr->type)) {
    let alloc = addAlloca(state, expr->type);
    addInstr(state, null, InstrKind::Store {
      ptr = alloc,
      val = res,
    });
    return alloc;
  }

  return res;
}
