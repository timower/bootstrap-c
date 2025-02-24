import state;
import expr;

import ir.type;

func genFunc(state: IRGenState*, decl: DeclAST*, fn: Function*) {
  state->counter = 0;

  newScope(state);

  state->curFunc = fn;
  state->curBB = addBasicBlock(state, "entry");

  let idx = 0;
  for (let arg = decl->fields; arg != null; arg = arg->next, idx++) {
    // TODO: just return Value?
    let alloc = addAlloca(state, arg->type);

    addLocal(state, arg->name, alloc);
    addInstr(state, null, InstrKind::Store {
      ptr = alloc,
      val = Value::Argument {
        idx = idx,
        type = arg->type,
      },
    });
  }

  genStmt(state, decl->body);

  // TODO: check if there's no terminator
  let fnType = fn->type->kind as TypeKind::Func*;
  if (fnType->result->kind as TypeKind::Void* != null) {
    addInstr(state, null, InstrKind::ReturnVoid {});
  } else {
    addInstr(state, null, InstrKind::Return {
      val = Value::Zero {
        type = fnType->result,
      },
    });
  }

  popScope(state);
}

func genStmt(state: IRGenState*, stmt: StmtAST*) {
  switch (stmt->kind) {
    case StmtKind::EXPR:
      if (stmt->expr != null) {
        genExpr(state, stmt->expr);
      }

    case StmtKind::COMPOUND:
      newScope(state);
      for (let cur = stmt->stmt; cur != null; cur = cur->nextStmt) {
        genStmt(state, cur);
      }
      popScope(state);

    case StmtKind::RETURN:
      if (stmt->expr == null) {
        addInstr(state, null, InstrKind::ReturnVoid {});
        return;
      }
      let v = genExpr(state, stmt->expr);
      if (stmt->expr->type->kind as TypeKind::Void* != null) {
        addInstr(state, null, InstrKind::ReturnVoid {});
        return;
      }

      if (isAggregate(stmt->expr->type)) {
        v = addInstr(state, stmt->expr->type, InstrKind::Load {
          ptr = v,
        });
      }
      addInstr(state, null, InstrKind::Return {
        val = v,
      });

    case StmtKind::IF:
      let cond = genExpr(state, stmt->expr);

      let trueBB = addBasicBlock(state, "if.true");
      let falseBB: BasicBlock* = stmt->stmt != null
           ? addBasicBlock(state, "if.false")
           : null as BasicBlock*;
      let contBB = addBasicBlock(state, "if.cont");
      let falseJmpBB = falseBB == null ? contBB : falseBB;
      addInstr(state, null, InstrKind::CondBranch {
        cond = cond,
        trueBB = trueBB,
        falseBB = falseJmpBB,
      });

      state->curBB = trueBB;
      genStmt(state, stmt->init);
      addInstr(state, null, InstrKind::Branch {
        bb = contBB,
      });

      if (falseBB != null) {
        state->curBB = falseBB;
        genStmt(state, stmt->stmt);
        addInstr(state, null, InstrKind::Branch {
          bb = contBB,
        });
      }

      state->curBB = contBB;

    case StmtKind::WHILE:
      let condBB = addBasicBlock(state, "while.cond");
      addInstr(state, null, InstrKind::Branch {
        bb = condBB,
      });

      state->curBB = condBB;
      let cond = genExpr(state, stmt->expr);
      let bodyBB = addBasicBlock(state, "while.body");
      let contBB = addBasicBlock(state, "while.cont");
      addInstr(state, null, InstrKind::CondBranch {
        cond = cond,
        trueBB = bodyBB,
        falseBB = contBB,
      });

      newScope(state);
      state->curBB = bodyBB;
      state->scope->breakBB = contBB;      // TODO: continue;

      genStmt(state, stmt->stmt);

      addInstr(state, null, InstrKind::Branch {
        bb = condBB,
      });

      popScope(state);
      state->curBB = contBB;

    case StmtKind::FOR:
      // Initialize the for loop
      genStmt(state, stmt->init);

      // Create basic blocks for all parts of the for loop
      let condBB = addBasicBlock(state, "for.cond");
      let bodyBB = addBasicBlock(state, "for.body");
      let incrBB = addBasicBlock(state, "for.incr");
      let contBB = addBasicBlock(state, "for.cont");

      // Branch to condition block
      addInstr(state, null, InstrKind::Branch {
        bb = condBB,
      });

      // Generate condition code
      state->curBB = condBB;
      let cond = genExpr(state, stmt->cond->expr);
      addInstr(state, null, InstrKind::CondBranch {
        cond = cond,
        trueBB = bodyBB,
        falseBB = contBB,
      });

      // Set up new scope for the loop body
      newScope(state);
      state->curBB = bodyBB;
      state->scope->breakBB = contBB;      // TODO: continue;

      // Generate the loop body
      genStmt(state, stmt->stmt);

      // Branch to increment block
      addInstr(state, null, InstrKind::Branch {
        bb = incrBB,
      });

      // Generate increment code
      state->curBB = incrBB;
      genExpr(state, stmt->expr);

      // Branch back to condition
      addInstr(state, null, InstrKind::Branch {
        bb = condBB,
      });

      // Clean up
      popScope(state);
      state->curBB = contBB;

    case StmtKind::BREAK:
      if (state->scope->breakBB == null) {
        printf("%s:%d:%d ", stmt->location.fileName, stmt->location.line, stmt->location.column);
        failIRGen("Break outside loop");
      }
      addInstr(state, null, InstrKind::Branch {
        bb = state->scope->breakBB,
      });

    case StmtKind::SWITCH:
      genSwitch(state, stmt);

    case StmtKind::CASE, StmtKind::DEFAULT:
      failIRGen("Case outside of switch");
  }
}

func newCase(cases: Case*, bb: BasicBlock*) -> Case* {
  let c = calloc(1, sizeof(struct Case)) as Case*;
  c->next = cases;
  c->bb = bb;
  return c;
}

func getCases(
    state: IRGenState*,
    expr: ExprAST*,
    unionAddr: Value*,
    cases: Case*,
    bb: BasicBlock*
) -> Case* {
  switch (expr->kind) {
    case ExprKind::SCOPE, ExprKind::INT:
      let cse = newCase(cases, bb);
      if (unionAddr != null) {
        cse->val = Value::IntConstant {
          value = expr->value,
          type = getInt32(),
        };
      } else {
        cse->val = genExpr(state, expr);
      }
      cse->next = cases;
      return cse;

    case ExprKind::BINARY:
      let lhsCases = getCases(state, expr->lhs, unionAddr, cases, bb);
      return getCases(state, expr->rhs, unionAddr, lhsCases, bb);

    case ExprKind::MEMBER:
      if (unionAddr == null) {
        failIRGen("case as on non union type?");
      }
      let val = addInstr(state, getPtrType(), InstrKind::StructGEP {
        type = expr->type,
        ptr = *unionAddr,
        field = 1,
      });
      addLocal(state, expr->identifier, val);

      let cse = newCase(cases, bb);
      cse->val = Value::IntConstant {
        value = expr->value,
        type = getInt32(),
      };
      return cse;

    default:
      failIRGen("Unsupported case expr");
  }
}

func genSwitch(state: IRGenState*, stmt: StmtAST*) {
  // Create basic blocks for switch
  let switchBB = addBasicBlock(state, "switch");
  let contBB = addBasicBlock(state, "switch.cont");

  // Set up new scope for switch
  newScope(state);
  state->scope->breakBB = contBB;

  let switchExpr = stmt->expr;
  let isUnion = switchExpr->type->kind as TypeKind::Union* != null;

  // Generate switch condition
  let expr: Value = Value::InstrPtr {};
  let unionAddrPtr: Value* = null;
  if (isUnion) {
    let unionAddr = genAddr(state, switchExpr);
    let unionType = switchExpr->type;

    // Load discriminant (first field)
    let gep = addInstr(state, getPtrType(), InstrKind::StructGEP {
      type = unionType,
      ptr = unionAddr,
      field = 0,
    });
    expr = addInstr(state, getInt32(), InstrKind::Load {
      ptr = gep,
    });
    unionAddrPtr = &unionAddr;
  } else {
    expr = genExpr(state, switchExpr);
  }

  // Branch to switch block
  addInstr(state, null, InstrKind::Branch {
    bb = switchBB,
  });

  // Generate case blocks and collect cases
  let cases: Case* = null;
  let defaultBB: BasicBlock* = null;

  for (let caseStmt = stmt->stmt; caseStmt != null;
       caseStmt = caseStmt->nextStmt) {
    newScope(state);

    if (caseStmt->kind == StmtKind::CASE) {
      let caseBB = addBasicBlock(state, "switch.case");
      state->curBB = caseBB;
      cases = getCases(state, caseStmt->expr, unionAddrPtr, cases, caseBB);
    } else if (caseStmt->kind == StmtKind::DEFAULT) {
      if (defaultBB != null) {
        failIRGen("Multiple default");
      }
      defaultBB = addBasicBlock(state, "switch.default");
      state->curBB = defaultBB;
    } else {
      failIRGen("Unsupported switch stmt");
    }

    // Generate case body
    for (let cur = caseStmt->stmt; cur != null; cur = cur->nextStmt) {
      genStmt(state, cur);
    }

    popScope(state);

    // Branch to continue block if no terminator
    addInstr(state, null, InstrKind::Branch {
      bb = contBB,
    });
  }

  if (defaultBB == null) {
    defaultBB = contBB;
  }

  // Generate switch instruction
  state->curBB = switchBB;
  addInstr(state, null, InstrKind::Switch {
    cond = expr,
    defaultBB = defaultBB,
    cases = cases,
  });

  // Clean up
  popScope(state);
  state->curBB = contBB;
}
