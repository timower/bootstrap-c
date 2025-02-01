import state;
import expr;

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
