import ir;
import ir.type;
import ast;
import ast.print;
import util;

import irgen.state;
import irgen.utils;

import sema.type;


func genConstant(state: IRGenState*, expr: ExprAST*) -> Value {
  switch (expr->kind) {
    case ExprKind::Int as intExpr:
      if (expr->type->kind as TypeKind::Pointer* != null) {
        if (intExpr.value != 0) {
          // sema doesn't allow us to geenrate this.
          unreachable("Only null constants supported");
        }
        return Value::Zero {
          type = expr->type,
        };
      }

      return Value::IntConstant {
        value = intExpr.value,
        type = expr->type,
      };

    case ExprKind::Paren as paren:
      return genConstant(state, paren.expr);

    case ExprKind::Scope as scopeExpr:
      return Value::IntConstant {
        value = scopeExpr.enumValue,
        type = expr->type,
      };

    case ExprKind::Str as strExpr:
      return Value::StrConstant {
        value = strExpr.identifier,
        type = expr->type,
      };

    case ExprKind::SliceIndex as sliceIdx:
      let slice = genConstant(state, sliceIdx.slice);
      if (sliceIdx.end == null || sliceIdx.start != null) {
        failIRGen("Constant GEP not supported yet");
      }
      let values = (calloc(2, sizeof(Value)) as Value*)[:2];
      values[0] = slice;
      values[1] = genConstant(state, sliceIdx.end);
      return Value::StructConstant {
        values = values,
        type = expr->type,
      };

    case ExprKind::Cast as cast:
      switch (cast.castKind) {
        case CastKind::Noop:
          return genConstant(state, cast.expr);
        default:
          break;
      }

    case ExprKind::Array as arrayExpr:
      let arrayType = expr->type->kind as TypeKind::Array*;
      let size = arrayType->size as u32;
      let valuesPtr = calloc(size as uptr, sizeof(union Value)) as Value*;
      let values = valuesPtr[:size];

      let i = 0;
      for (let field = arrayExpr.elements; field != null; field = field->next, i++) {
        values[i] = genConstant(state, field);
      }
      return Value::ArrayConstant {
        type = expr->type,
        values = values,
      };

    // Address of global
    case ExprKind::Unary as unaryExpr:
      if (unaryExpr.op.kind != TokenKind::AND) {
        break;
      }
      let globalVar = unaryExpr.prefix;
      let varExpr = globalVar->kind as ExprKind::Variable*;
      if (varExpr == null) {
        break;
      }
      let var = findName(state, varExpr->identifier);
      if (var == null) {
        // sema should fail here.
        unreachable("No variable in scope");
      }

      if (let globalPtr = var as Value::GlobalPtr*) {
        return *var;
      }

      if (let funcPtr = var as Value::FuncPtr*) {
        return *var;
      }

      // There's no way to get here, alll addLocal calls are funcs or globals
      unreachable("Non func or global at globall scope");

    // Binary expressions are now handled in sema via evalConstant
    // genConstant should only receive pre-evaluated constant expressions
    default:
      break;
  }

  printExpr(expr);
  printf("\n");
  failIRGen("TODO: constant exprs");
  return Value::Zero {};
}

func genAddr(state: IRGenState*, expr: ExprAST*) -> Value {
  switch (expr->kind) {
    case ExprKind::Variable as varExpr:
      let var = findName(state, varExpr.identifier);
      if (var == null) {
        unreachable("Failed to find variable");
      }
      return *var;

    case ExprKind::Unary as unaryExpr:
      if (unaryExpr.op.kind == TokenKind::STAR) {
        return genExpr(state, unaryExpr.prefix);
      }

    case ExprKind::Index as indexExpr:
      let array: Value = Value::Zero {};
      switch (indexExpr.array->type->kind) {
        case TypeKind::Slice:
          let sliceVal = genExpr(state, indexExpr.array);
          array = addInstr(state, getPtrType(), InstrKind::Load {
            ptr = addInstr(state, getPtrType(), InstrKind::StructGEP {
              type = indexExpr.array->type,
              ptr = sliceVal,
              field = 0,
            }),
          });
        case TypeKind::Array:
          array = genAddr(state, indexExpr.array);
        default:
          unreachable("Unsupported index array type");
      }

      let index = genExpr(state, indexExpr.index);
      return addInstr(state, getPtrType(), InstrKind::ArrayGEP {
        type = expr->type,
        ptr = array,
        idx = index,
      });

    case ExprKind::Member as memberExpr:
      let agg = memberExpr.op.kind == TokenKind::DOT
           ? genAddr(state, memberExpr.object)
           : genExpr(state, memberExpr.object);

      let aggType: Type* = null;
      if (memberExpr.op.kind == TokenKind::DOT) {
        aggType = memberExpr.object->type;
      } else {
        aggType = (memberExpr.object->type->kind as TypeKind::Pointer*)->pointee;
      }
      return addInstr(state, getPtrType(), InstrKind::StructGEP {
        type = aggType,
        ptr = agg,
        field = memberExpr.fieldIndex,
      });

    case ExprKind::Paren as parenExpr:
      return genAddr(state, parenExpr.expr);

    case ExprKind::GenericInstantiation as genericInst:
      let var = findName(state, genericInst.instance);
      if (var == null) {
        unreachable("Failed to find function");
      }
      return *var;

    case ExprKind::Call:
      if (!isAggregate(expr->type)) {
        break;
      }
      return genExpr(state, expr);

    default:
      break;
  }

  printLoc(expr->location);
  failIRGen("Expr can't be used as lvalue");
  printExpr(expr);
  printf("\n");
  return Value::InstrPtr {};
}


func genExpr(state: IRGenState*, expr: ExprAST*) -> Value {
  switch (expr->kind) {
    case ExprKind::Paren as parenExpr:
      return genExpr(state, parenExpr.expr);

    case ExprKind::Int, ExprKind::Scope:
      return genConstant(state, expr);

    case ExprKind::Variable, ExprKind::Index, ExprKind::Member,
         ExprKind::GenericInstantiation:
      let addr = genAddr(state, expr);
      return genLoad(state, addr, expr->type);

    case ExprKind::SliceIndex as slice:
      return genSliceIndex(state, expr);

    case ExprKind::Unary:
      return genUnary(state, expr);

    case ExprKind::Binary as binExpr:
      if (isAssign(binExpr.op)) {
        return genAssign(state, expr);
      }

      if (binExpr.op.kind == TokenKind::AND_OP || binExpr.op.kind == TokenKind::OR_OP) {
        return genLogicalBinOp(state, expr);
      }

      let lhs = genExpr(state, binExpr.lhs);
      let rhs = genExpr(state, binExpr.rhs);

      if (binExpr.op.kind == TokenKind::COMMA) {
        return rhs;
      }

      return genBinary(
          state,
          expr->type,
          binExpr.op.kind,
          lhs,
          binExpr.lhs->type,
          rhs,
          binExpr.rhs->type);

    case ExprKind::Conditional:
      return genConditional(state, expr);

    case ExprKind::Call:
      return genCall(state, expr);

    case ExprKind::Struct:
      return genStructExpr(state, expr);
    case ExprKind::Array:
      return genArrayExpr(state, expr);

    case ExprKind::Cast:
      return genCast(state, expr);

    case ExprKind::Let as letExpr:
      switch (letExpr.decl->kind) {
        case DeclKind::Var as varKind:
          let initVal = genExpr(state, varKind.init);

          let alloc = initVal;
          if (needsAlloc(varKind.init)) {
            alloc = addAlloca(state, expr->type);
            genStore(state, alloc, initVal, expr->type);
          }

          addLocal(state, letExpr.decl->name, alloc);

          return initVal;

        case DeclKind::Const as constKind:
          // Const expressions are handled during sema.
          return genExpr(state, constKind.init);
        default:
          // Parsing doesn't allow this to happen.
          unreachable("Invalid decl kind in let expression");
          return Value::Zero {};
      }

    case ExprKind::Str:
      // Sema should've transformed these.
      unreachable("Strings should only be globals");
      return Value::Zero {};

    case ExprKind::Sizeof:
      // Sema should've transformed these.
      unreachable("Invalid expr");
      return Value::Zero {};
  }
}

func genSliceIndex(state: IRGenState*, expr: ExprAST*) -> Value {
  let slice = expr->kind as ExprKind::SliceIndex*;

  let iptrType = getIPtr(&state->module.target);

  let sliceType = expr->type;
  let sliceKind = sliceType->kind as TypeKind::Slice*;

  let alloc = addAlloca(state, sliceType);
  let dataPtr = addInstr(state, getPtrType(), InstrKind::StructGEP {
    type = sliceType,
    ptr = alloc,
    field = 0,
  });
  let sizePtr = addInstr(state, getPtrType(), InstrKind::StructGEP {
    type = sliceType,
    ptr = alloc,
    field = 1,
  });

  let ptrVal: Value = Value::Zero {};
  let sizeVal: Value = Value::Zero {};

  switch (slice->slice->type->kind) {
    case TypeKind::Array as a:
      let sliceVal = genAddr(state, slice->slice);
      sizeVal = Value::IntConstant {
        value = a.size,
        type = iptrType,
      };
      ptrVal = sliceVal;

    case TypeKind::Pointer:
      ptrVal = genExpr(state, slice->slice);
      sizeVal = Value::IntConstant {
        value = 0,
        type = iptrType,
      };

    case TypeKind::Slice:
      let sliceVal = genExpr(state, slice->slice);
      ptrVal = addInstr(state, getPtrType(), InstrKind::Load {
        ptr = addInstr(state, getPtrType(), InstrKind::StructGEP {
          type = sliceType,
          ptr = sliceVal,
          field = 0,
        }),
      });
      sizeVal = addInstr(state, iptrType, InstrKind::Load {
        ptr = addInstr(state, getPtrType(), InstrKind::StructGEP {
          type = sliceType,
          ptr = sliceVal,
          field = 1,
        }),
      });

    default:
      // Sema shoud reject any other types.
      unreachable("Unsupported slice index operation");
  }

  let startVal: Value = Value::IntConstant {
    type = iptrType,
    value = 0,
  };
  if (slice->start != null) {
    startVal = genExpr(state, slice->start);
    ptrVal = addInstr(state, getPtrType(), InstrKind::ArrayGEP {
      type = sliceKind->element,
      ptr = ptrVal,
      idx = startVal,
    });
  }

  if (slice->end != null) {
    let endVal = genExpr(state, slice->end);
    if (slice->start != null) {
      sizeVal = addInstr(state, slice->end->type, InstrKind::Binary {
        op = BinaryOp::Sub,
        lhs = endVal,
        rhs = startVal,
      });
    } else {
      sizeVal = endVal;
    }
  } else if (slice->start != null) {
    sizeVal = addInstr(state, slice->start->type, InstrKind::Binary {
      op = BinaryOp::Sub,
      lhs = sizeVal,
      rhs = startVal,
    });
  }

  addInstr(state, null, InstrKind::Store {
    ptr = dataPtr,
    val = ptrVal,
  });
  addInstr(state, null, InstrKind::Store {
    ptr = sizePtr,
    val = sizeVal,
  });

  return alloc;
}

func genUnary(state: IRGenState*, expr: ExprAST*) -> Value {
  let unaryExpr = expr->kind as ExprKind::Unary*;
  switch (unaryExpr->op.kind) {
    case TokenKind::STAR:
      let addr = genExpr(state, unaryExpr->prefix);
      return genLoad(state, addr, expr->type);

    case TokenKind::AND:
      return genAddr(state, unaryExpr->prefix);

    case TokenKind::INC_OP, TokenKind::DEC_OP:
      let opExpr = unaryExpr->postfix == null ? unaryExpr->prefix : unaryExpr->postfix;
      let operand = genAddr(state, opExpr);
      let val = genLoad(state, operand, opExpr->type);
      let type = opExpr->type;
      let one = Value::IntConstant {
        value = unaryExpr->op.kind == TokenKind::INC_OP ? 1 : -1,
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
      if (unaryExpr->postfix != null) {
        return val;
      }
      return res;

    case TokenKind::PLUS:
      return genExpr(state, unaryExpr->prefix);
    case TokenKind::MINUS:
      let op = genExpr(state, unaryExpr->prefix);
      return addInstr(state, expr->type, InstrKind::Binary {
        op = BinaryOp::Sub,
        lhs = Value::IntConstant {
          value = 0,
          type = expr->type,
        },
        rhs = op,
      });

    case TokenKind::TILDE:
      let op = genExpr(state, unaryExpr->prefix);
      return addInstr(state, expr->type, InstrKind::Binary {
        op = BinaryOp::Xor,
        lhs = op,
        rhs = Value::IntConstant {
          value = -1,
          type = getInt32(),
        },
      });

    case TokenKind::BANG:
      let op = genExpr(state, unaryExpr->prefix);
      return addInstr(state, expr->type, InstrKind::Cmp {
        op = CmpOp::Eq,
        lhs = op,
        rhs = Value::IntConstant {
          value = 0,
          type = getBool(),
        },
      });

    default:
      // The parser should not accept this, and sema should fail before this.
      unreachable("Invalid unary");
      return Value::Zero {};
  }
}

func genCast(state: IRGenState*, expr: ExprAST*) -> Value {
  let castExpr = expr->kind as ExprKind::Cast*;

  let v = genExpr(state, castExpr->expr);

  let from = castExpr->expr->type;
  let to = expr->type;
  switch (castExpr->castKind) {
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
        value = castExpr->fieldIndex,
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
          value = castExpr->fieldIndex,
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

    case CastKind::Trunc, CastKind::Sext, CastKind::Zext:
      return addInstr(state, expr->type, InstrKind::Cast {
        kind = castExpr->castKind,
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
  let isBinOp = true;
  let binOp = BinaryOp::Add;
  let cmpOp = CmpOp::Eq;

  let intResType = resType->kind as TypeKind::Int*;

  switch (opKind) {
    default:
      // We don't parse anything not in this list.
      unreachable("Invalid binary op");
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
      binOp = intResType->isSigned ? BinaryOp::AShr : BinaryOp::LShr;
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
  let binExpr = expr->kind as ExprKind::Binary*;
  let lhs = genExpr(state, binExpr->lhs);
  let entryBB = state->curBB;

  let trueBB = addBasicBlock(state, "true");
  let falseBB = addBasicBlock(state, "false");

  let firstBB = trueBB;
  let secondBB = falseBB;

  let falseResult = 0;
  if (binExpr->op.kind == TokenKind::OR_OP) {
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
  let rhs = genExpr(state, binExpr->rhs);
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
  let binExpr = expr->kind as ExprKind::Binary*;
  let adr = genAddr(state, binExpr->lhs);
  let val = genExpr(state, binExpr->rhs);

  if (binExpr->op.kind == TokenKind::EQ) {
    genStore(state, adr, val, expr->type);
    return val;
  }

  let lval = genLoad(state, adr, binExpr->lhs->type);

  let op = TokenKind::TOK_EOF;
  switch (binExpr->op.kind) {
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
      unreachable("Invalid assign op");
  }

  let res = genBinary(
      state,
      expr->type,
      op,
      lval,
      binExpr->lhs->type,
      val,
      binExpr->rhs->type);

  genStore(state, adr, res, expr->type);
  return res;
}

func genConditional(state: IRGenState*, expr: ExprAST*) -> Value {
  let condExpr = expr->kind as ExprKind::Conditional*;
  let cond = genExpr(state, condExpr->cond);

  let trueBB = addBasicBlock(state, "true");
  let falseBB = addBasicBlock(state, "false");
  let contBB = addBasicBlock(state, "cont");

  addInstr(state, null, InstrKind::CondBranch {
    cond = cond,
    trueBB = trueBB,
    falseBB = falseBB,
  });

  state->curBB = trueBB;
  let trueVal = genExpr(state, condExpr->trueExpr);
  addInstr(state, null, InstrKind::Branch {
    bb = contBB,
  });
  let trueExitBB = state->curBB;

  state->curBB = falseBB;
  let falseVal = genExpr(state, condExpr->falseExpr);
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
  let structExpr = expr->kind as ExprKind::Struct*;
  let res = addAlloca(state, expr->type);
  addInstr(state, null, InstrKind::Store {
    ptr = res,
    val = Value::Zero {
      type = expr->type,
    },
  });

  for (let field = structExpr->fieldIndices; field != null; field = field->next) {
    let fieldVal = genExpr(state, field->value);
    let fieldGep =
        addInstr(state, getPtrType(), InstrKind::StructGEP {
      type = expr->type,
      ptr = res,
      field = field->index,
    });
    genStore(state, fieldGep, fieldVal, field->value->type);
  }

  return res;
}

func genArrayExpr(state: IRGenState*, expr: ExprAST*) -> Value {
  let structExpr = expr->kind as ExprKind::Array*;
  let arrayType = expr->type->kind as TypeKind::Array*;

  let res = addAlloca(state, expr->type);
  let idx = 0;
  for (let elem = structExpr->elements; elem != null; elem = elem->next, idx++) {
    let fieldVal = genExpr(state, elem);
    let fieldGep =
        addInstr(state, getPtrType(), InstrKind::ArrayGEP {
      type = arrayType->element,
      ptr = res,
      idx = Value::IntConstant {
        value = idx,
        type = getInt32(),
      },
    });
    genStore(state, fieldGep, fieldVal, elem->type);
  }

  return res;
}

func genCall(state: IRGenState*, expr: ExprAST*) -> Value {
  let callExpr = expr->kind as ExprKind::Call*;
  let numArgs = 0 as uptr;
  for (let arg = callExpr->args; arg != null; arg = arg->next) {
    numArgs++;
  }

  let args = (calloc(numArgs, sizeof(union Value)) as Value*)[:numArgs];
  let i = 0;
  for (let arg = callExpr->args; arg != null; arg = arg->next, i++) {
    let argVal = genExpr(state, arg);
    if (isAggregate(arg->type)) {
      argVal
          = addInstr(state, arg->type, InstrKind::Load {
          ptr = argVal,
        });
    }
    args[i] = argVal;
  }

  let fn = genExpr(state, callExpr->function);
  let fnType = getFunctionType(callExpr);

  let res = addInstr(state, expr->type, InstrKind::Call {
    fn = fn,
    fnType = newType(*fnType),
    args = args,
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
