import state;
import expr;

import ir.type;

func genFunc(state: IRGenState*, decl: DeclAST*, fn: Function*) {
  state->counter = 0;

  newScope(state);

  state->cleanupSlot = Value::Zero {};
  state->retSlot = Value::Zero {};
  state->retBlock = null;

  state->curFunc = fn;
  state->curBB = addBasicBlock(state, "entry", decl->location);

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
  popScope(state);

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

  if (state->retBlock != null) {
    state->curBB = state->retBlock;

    let v: Value = Value::Zero {};
    if (fnType->result->kind as TypeKind::Void* == null) {
      v = addInstr(state, fnType->result, InstrKind::Load {
        ptr = state->retSlot,
      });
    }

    genReturn(state, v, fnType->result);
  }
}

func hasCleanup(state: IRGenState*) -> bool {
  return state->cleanupSlot as Value::Zero* == null;
}

func addCleanup(state: IRGenState*, deferStmt: StmtAST*) {
  let res = alloc(state->localAlloc, sizeof(struct Cleanup)) as Cleanup*;
  res->bb = addBasicBlock(state, "cleanup", deferStmt->location);
  res->stmt = deferStmt;
  res->next = state->scope->cleanups;
  state->scope->cleanups = res;

  if (!hasCleanup(state)) {
    let val = addAlloca(state, getInt32(state->irAlloc));

    let alloc = val as Value::AllocaPtr*;
    alloc->ptr->dbgName = "cleanupslot";

    state->cleanupSlot = val;
  }
}


func genCleanup(state: IRGenState*) {
  for (let cleanup = state->scope->cleanups;
       cleanup != null; cleanup = cleanup->next) {
    state->curBB = cleanup->bb;
    genStmt(state, cleanup->stmt);

    let v = addInstr(state, getInt32(state->irAlloc), InstrKind::Load {
      ptr = state->cleanupSlot,
    });

    addInstr(state, null, InstrKind::Switch {
      cond = v,
      defaultBB = cleanup->cases->bb,
      cases = cleanup->cases->next,
    });
  }
}

func popScope(state: IRGenState*) {
  if (state->scope->cleanups != null) {
    let targetBB = addBasicBlock(state, "cont", state->curBB->location);
    genBranch(state, JmpSlot {
      bb = targetBB,
      scope = state->scope->parent,
    });

    genCleanup(state);
    state->curBB = targetBB;
  }

  state->scope = state->scope->parent;
}

func genBranch(state: IRGenState*, target: JmpSlot) {
  let firstCleanup: BasicBlock* = null;
  let lastCleanup: Cleanup* = null;

  let cleanupId = state->cleanupCounter;
  let cleanupVal = Value::IntConstant {
    value = cleanupId,
    type = getInt32(state->irAlloc),
  };

  for (let scope = state->scope; scope != target.scope; scope = scope->parent) {
    for (let cleanup = scope->cleanups; cleanup != null; cleanup = cleanup->next) {
      if (firstCleanup == null) {
        firstCleanup = cleanup->bb;
      }

      if (lastCleanup != null) {
        let newCase = newCase(state->irAlloc, lastCleanup->cases, cleanup->bb);
        newCase->val = cleanupVal;
        lastCleanup->cases = newCase;
      }

      lastCleanup = cleanup;
    }
  }

  if (lastCleanup != null) {
    let newCase = newCase(state->irAlloc, lastCleanup->cases, target.bb);
    newCase->val = cleanupVal;
    lastCleanup->cases = newCase;
  }

  if (firstCleanup == null) {
    addInstr(state, null, InstrKind::Branch {
      bb = target.bb,
    });
  } else {
    addInstr(state, null, InstrKind::Store {
      ptr = state->cleanupSlot,
      val = cleanupVal,
    });
    addInstr(state, null, InstrKind::Branch {
      bb = firstCleanup,
    });
    state->cleanupCounter++;
  }
}

func getRetSlot(state: IRGenState*, type: Type*) -> Value {
  if (state->retSlot as Value::Zero* != null) {
    state->retSlot = addAlloca(state, type);

    let alloc = state->retSlot as Value::AllocaPtr*;
    alloc->ptr->dbgName = "retSlot";
  }
  return state->retSlot;
}

func getRetBlock(state: IRGenState*, loc: SourceLoc*) -> BasicBlock* {
  if (state->retBlock == null) {
    state->retBlock = addBasicBlock(state, "ret", loc);
  }
  return state->retBlock;
}

func genReturn(state: IRGenState*, val: Value, type: Type*) {
  if (type == null || type->kind as TypeKind::Void* != null) {
    addInstr(state, null, InstrKind::ReturnVoid {});
    return;
  }

  addInstr(state, null, InstrKind::Return {
    val = val,
  });
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
      let t: Type* = null;
      let v: Value = Value::Zero {};
      if (retStmt.expr != null) {
        t = retStmt.expr->type;
        v = genExpr(state, retStmt.expr);
        if (hasCleanup(state)) {
          genStore(state, getRetSlot(state, t), v, t);
        }
      }

      if (hasCleanup(state)) {
        genBranch(state, JmpSlot {
          bb = getRetBlock(state, stmt->location),
          scope = null,
        });
      } else {
        if (t != null && isAggregate(t)) {
          v = addInstr(state, t, InstrKind::Load {
            ptr = v,
          });
        }
        genReturn(state, v, t);
      }

    case StmtKind::If as ifStmt:
      let cond = genExpr(state, ifStmt.cond);

      let trueBB = addBasicBlock(state, "if.true", ifStmt.thenStmt->location);
      let falseBB: BasicBlock* = ifStmt.elseStmt != null
           ? addBasicBlock(state, "if.false", ifStmt.elseStmt->location)
           : null as BasicBlock*;
      let contBB = addBasicBlock(state, "if.cont", stmt->endLocation);
      let falseJmpBB = falseBB == null ? contBB : falseBB;
      addInstr(state, null, InstrKind::CondBranch {
        cond = cond,
        trueBB = trueBB,
        falseBB = falseJmpBB,
      });

      state->curBB = trueBB;
      newScope(state);
      genStmt(state, ifStmt.thenStmt);
      popScope(state);
      addInstr(state, null, InstrKind::Branch {
        bb = contBB,
      });

      if (falseBB != null) {
        state->curBB = falseBB;
        newScope(state);
        genStmt(state, ifStmt.elseStmt);
        popScope(state);
        addInstr(state, null, InstrKind::Branch {
          bb = contBB,
        });
      }

      state->curBB = contBB;

    case StmtKind::While as whileStmt:
      let condBB = addBasicBlock(state, "while.cond", stmt->location);
      addInstr(state, null, InstrKind::Branch {
        bb = condBB,
      });

      state->curBB = condBB;
      let cond = genExpr(state, whileStmt.cond);
      let bodyBB = addBasicBlock(state, "while.body", whileStmt.body->location);
      let contBB = addBasicBlock(state, "while.cont", stmt->endLocation);
      addInstr(state, null, InstrKind::CondBranch {
        cond = cond,
        trueBB = bodyBB,
        falseBB = contBB,
      });

      let oldScope = state->scope;
      newScope(state);
      state->curBB = bodyBB;
      state->scope->breakSlot = JmpSlot {
        bb = contBB,
        scope = oldScope,
      };
      state->scope->continueSlot = JmpSlot {
        bb = condBB,
        scope = oldScope,
      };

      genStmt(state, whileStmt.body);
      popScope(state);

      addInstr(state, null, InstrKind::Branch {
        bb = condBB,
      });

      state->curBB = contBB;

    case StmtKind::For as forStmt:
      // Initialize the for loop
      genStmt(state, forStmt.init);

      // Create basic blocks for all parts of the for loop
      let condBB = addBasicBlock(state, "for.cond", stmt->location);
      let bodyBB = addBasicBlock(state, "for.body", forStmt.body->location);
      let incrBB = addBasicBlock(state, "for.incr", stmt->location);
      let contBB = addBasicBlock(state, "for.cont", stmt->endLocation);

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
      let oldScope = state->scope;
      newScope(state);
      state->curBB = bodyBB;
      state->scope->breakSlot = JmpSlot {
        bb = contBB,
        scope = oldScope,
      };
      state->scope->continueSlot = JmpSlot {
        bb = incrBB,
        scope = oldScope,
      };

      // Generate the loop body
      genStmt(state, forStmt.body);

      // Branch to increment block
      addInstr(state, null, InstrKind::Branch {
        bb = incrBB,
      });

      // Generate increment code
      state->curBB = incrBB;
      genExpr(state, forStmt.update);

      // Clean up
      popScope(state);

      // Branch back to condition
      addInstr(state, null, InstrKind::Branch {
        bb = condBB,
      });

      state->curBB = contBB;

    case StmtKind::Defer as d:
      addCleanup(state, d.stmt);

    case StmtKind::Break:
      if (state->scope->breakSlot.bb == null) {
        failIRGen(state, "Break outside loop");
      }

      //addInstr(state, null, InstrKind::Branch {
      //  bb = state->scope->breakBB,
      //});
      genBranch(state, state->scope->breakSlot);

    case StmtKind::Continue:
      if (state->scope->continueSlot.bb == null) {
        failIRGen(state, "Continue outside loop");
      }

      // addInstr(state, null, InstrKind::Branch {
      //   bb = state->scope->continueBB,
      // });
      genBranch(state, state->scope->continueSlot);

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

func newCase(allocator: Allocator*, cases: Case*, bb: BasicBlock*) -> Case* {
  let c = alloc(allocator, sizeof(struct Case)) as Case*;
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
      let cse = newCase(state->irAlloc, cases, bb);
      cse->val = Value::IntConstant {
        value = scopeExpr.enumValue,
        type = getInt32(state->irAlloc),
      };

      return cse;

    case ExprKind::Binary as binary:
      if (binary.op.kind != TokenKind::COMMA) {
        unreachable("Only comma expression supported in case");
      }
      let lhsCases = getCases(state, binary.lhs, unionAddr, cases, bb);
      return getCases(state, binary.rhs, unionAddr, lhsCases, bb);

    case ExprKind::Int as intExpr:
      let cse = newCase(state->irAlloc, cases, bb);
      cse->val = genConstant(state, expr);
      return cse;

    case ExprKind::Member as memberExpr:
      if (unionAddr == null) {
        unreachable("case as on non union type?");
      }
      let val = addInstr(state, getPtrType(state->irAlloc), InstrKind::StructGEP {
        type = expr->type,
        ptr = *unionAddr,
        field = 1,
      });
      addLocal(state, memberExpr.identifier, val);

      let cse = newCase(state->irAlloc, cases, bb);
      cse->val = Value::IntConstant {
        value = memberExpr.fieldIndex,
        type = getInt32(state->irAlloc),
      };
      return cse;

    default:
      unreachable("Unsupported case expr");
      return null;
  }
}

func genSwitch(state: IRGenState*, stmt: StmtAST*) {
  // Create basic blocks for switch
  let switchBB = addBasicBlock(state, "switch", stmt->location);
  let contBB = addBasicBlock(state, "switch.cont", stmt->endLocation);

  // Set up new scope for switch
  let breakScope = state->scope;
  newScope(state);
  state->scope->breakSlot = JmpSlot {
    bb = contBB,
    scope = breakScope,
  };

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
    let gep = addInstr(state, getPtrType(state->irAlloc), InstrKind::StructGEP {
      type = unionType,
      ptr = unionAddr,
      field = 0,
    });
    expr = addInstr(state, getInt32(state->irAlloc), InstrKind::Load {
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
        let caseBB = addBasicBlock(state, "switch.case", caseStmt->location);
        state->curBB = caseBB;

        cases = getCases(state, cs.expr, unionAddrPtr, cases, caseBB);

        for (let cur = cs.body; cur != null; cur = cur->next) {
          genStmt(state, cur);
        }

      case StmtKind::Default as defKind:
        if (defaultBB != null) {
          failIRGen(state, "Multiple default");
        }
        defaultBB = addBasicBlock(state, "switch.default", caseStmt->location);
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
    defaultBB = addBasicBlock(state, "switch.default.trap", stmt->location);
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
