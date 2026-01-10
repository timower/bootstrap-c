import state;
import expr;

import ir.type;

func genFunc(state: IRGenState*, decl: DeclAST*, fn: Function*) {
  state->counter = 0;

  newScope(state);

  state->curFunc = fn;
  state->curBB = addBasicBlock(state, "entry");

  let idx = 0;
  for (let arg = (&decl->kind as DeclKind::Func*)->args; arg != null; arg = arg->next, idx++) {
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

  genStmt(state, (&decl->kind as DeclKind::Func*)->body);

  let fnType = fn->type->kind as TypeKind::Func*;
  if (fnType->result->kind as TypeKind::Void* != null) {
    addInstr(state, null, InstrKind::ReturnVoid {});
  } else if (state->curBB->begin == null) {
    addInstr(state, null, InstrKind::Call {
      fnType = state->intrinsics.trap->type,
      fn = Value::FuncPtr {
        ptr = state->intrinsics.trap,
      },
    });
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
    case StmtKind::Expr as exprStmt:
      if (exprStmt.expr != null) {
        genExpr(state, exprStmt.expr);
      }

    case StmtKind::Compound as compStmt:
      newScope(state);
      for (let cur = compStmt.stmt; cur != null; cur = cur->next) {
        genStmt(state, cur);
      }
      popScope(state);

    case StmtKind::Return as retStmt:
      if (retStmt.expr == null) {
        addInstr(state, null, InstrKind::ReturnVoid {});
        return;
      }
      let v = genExpr(state, retStmt.expr);
      if (&retStmt.expr->type->kind as TypeKind::Void* != null) {
        addInstr(state, null, InstrKind::ReturnVoid {});
        return;
      }

      if (isAggregate(retStmt.expr->type)) {
        v = addInstr(state, retStmt.expr->type, InstrKind::Load {
          ptr = v,
        });
      }
      addInstr(state, null, InstrKind::Return {
        val = v,
      });

    case StmtKind::If as ifStmt:
      let cond = genExpr(state, ifStmt.cond);

      let trueBB = addBasicBlock(state, "if.true");
      let falseBB: BasicBlock* = ifStmt.elseStmt != null
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
      genStmt(state, ifStmt.thenStmt);
      addInstr(state, null, InstrKind::Branch {
        bb = contBB,
      });

      if (falseBB != null) {
        state->curBB = falseBB;
        genStmt(state, ifStmt.elseStmt);
        addInstr(state, null, InstrKind::Branch {
          bb = contBB,
        });
      }

      state->curBB = contBB;

    case StmtKind::While as whileStmt:
      let condBB = addBasicBlock(state, "while.cond");
      addInstr(state, null, InstrKind::Branch {
        bb = condBB,
      });

      state->curBB = condBB;
      let cond = genExpr(state, whileStmt.cond);
      let bodyBB = addBasicBlock(state, "while.body");
      let contBB = addBasicBlock(state, "while.cont");
      addInstr(state, null, InstrKind::CondBranch {
        cond = cond,
        trueBB = bodyBB,
        falseBB = contBB,
      });

      newScope(state);
      state->curBB = bodyBB;
      state->scope->breakBB = contBB;
      state->scope->continueBB = condBB;

      genStmt(state, whileStmt.body);

      addInstr(state, null, InstrKind::Branch {
        bb = condBB,
      });

      popScope(state);
      state->curBB = contBB;

    case StmtKind::For as forStmt:
      // Initialize the for loop
      genStmt(state, forStmt.init);

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
      let condExpr = (&forStmt.cond->kind as StmtKind::Expr*)->expr;
      let cond = genExpr(state, condExpr);
      addInstr(state, null, InstrKind::CondBranch {
        cond = cond,
        trueBB = bodyBB,
        falseBB = contBB,
      });

      // Set up new scope for the loop body
      newScope(state);
      state->curBB = bodyBB;
      state->scope->breakBB = contBB;
      state->scope->continueBB = incrBB;

      // Generate the loop body
      genStmt(state, forStmt.body);

      // Branch to increment block
      addInstr(state, null, InstrKind::Branch {
        bb = incrBB,
      });

      // Generate increment code
      state->curBB = incrBB;
      genExpr(state, forStmt.update);

      // Branch back to condition
      addInstr(state, null, InstrKind::Branch {
        bb = condBB,
      });

      // Clean up
      popScope(state);
      state->curBB = contBB;

    case StmtKind::Break:
      if (state->scope->breakBB == null) {
        failIRGen("Break outside loop");
      }
      addInstr(state, null, InstrKind::Branch {
        bb = state->scope->breakBB,
      });

    case StmtKind::Continue:
      if (state->scope->continueBB == null) {
        failIRGen("Continue outside loop");
      }
      addInstr(state, null, InstrKind::Branch {
        bb = state->scope->continueBB,
      });

    case StmtKind::Switch:
      genSwitch(state, stmt);

    case StmtKind::Case:
      // sema would've caught this
      unreachable("Case outside of switch");
    case StmtKind::Default:
      // sema would've caught this
      unreachable("Default outside of switch");
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
    case ExprKind::Scope as scopeExpr:
      let cse = newCase(cases, bb);
      if (unionAddr != null) {
        cse->val = Value::IntConstant {
          value = scopeExpr.enumValue,
          type = getInt32(),
        };
      } else {
        cse->val = genConstant(state, expr);
      }
      return cse;

    case ExprKind::Binary as binary:
      if (binary.op.kind != TokenKind::COMMA) {
        unreachable("Only comma expression supported in case");
      }
      let lhsCases = getCases(state, binary.lhs, unionAddr, cases, bb);
      return getCases(state, binary.rhs, unionAddr, lhsCases, bb);

    case ExprKind::Int as intExpr:
      let cse = newCase(cases, bb);
      cse->val = genConstant(state, expr);
      return cse;

    case ExprKind::Member as memberExpr:
      if (unionAddr == null) {
        unreachable("case as on non union type?");
      }
      let val = addInstr(state, getPtrType(), InstrKind::StructGEP {
        type = expr->type,
        ptr = *unionAddr,
        field = 1,
      });
      addLocal(state, memberExpr.identifier, val);

      let cse = newCase(cases, bb);
      cse->val = Value::IntConstant {
        value = memberExpr.fieldIndex,
        type = getInt32(),
      };
      return cse;

    default:
      unreachable("Unsupported case expr");
      return null;
  }
}

func genSwitch(state: IRGenState*, stmt: StmtAST*) {
  // Create basic blocks for switch
  let switchBB = addBasicBlock(state, "switch");
  let contBB = addBasicBlock(state, "switch.cont");

  // Set up new scope for switch
  newScope(state);
  state->scope->breakBB = contBB;

  let switchStmt = &stmt->kind as StmtKind::Switch*;
  let switchExpr = switchStmt->expr;
  let isUnion = &switchExpr->type->kind as TypeKind::Union* != null;

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

  for (let caseStmt = switchStmt->body; caseStmt != null; caseStmt = caseStmt->next) {
    newScope(state);

    switch (caseStmt->kind) {
      case StmtKind::Case as cs:
        let caseBB = addBasicBlock(state, "switch.case");
        state->curBB = caseBB;

        cases = getCases(state, cs.expr, unionAddrPtr, cases, caseBB);

        for (let cur = cs.body; cur != null; cur = cur->next) {
          genStmt(state, cur);
        }

      case StmtKind::Default as defKind:
        if (defaultBB != null) {
          failIRGen("Multiple default");
        }
        defaultBB = addBasicBlock(state, "switch.default");
        state->curBB = defaultBB;

        for (let cur = defKind.body; cur != null; cur = cur->next) {
          genStmt(state, cur);
        }

      default:
        // parseSwitch doesn't make anything else
        unreachable("Unsupported switch stmt");
    }

    popScope(state);

    // Branch to continue block if no terminator
    addInstr(state, null, InstrKind::Branch {
      bb = contBB,
    });
  }

  if (defaultBB == null) {
    defaultBB = addBasicBlock(state, "switch.default.trap");
    state->curBB = defaultBB;
    addInstr(state, null, InstrKind::Call {
      fnType = state->intrinsics.trap->type,
      fn = Value::FuncPtr {
        ptr = state->intrinsics.trap,
      },
    });
    addInstr(state, null, InstrKind::Branch {
      bb = contBB,
    });
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
